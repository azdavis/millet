use crate::error::{ErrorKind, Item};
use crate::pat_match::{Con, Lang, Pat, VariantName};
use crate::st::{ErrorsMarker, St};
use crate::ty;
use crate::types::{Cx, IdStatus, Ty, TyScheme, ValEnv, ValInfo};
use crate::unify::unify;
use crate::util::{apply, cannot_bind_val, get_env, get_scon, ins_no_dupe, instantiate, record};

pub(crate) fn get(
  st: &mut St,
  cx: &Cx,
  ars: &hir::Arenas,
  ve: &mut ValEnv,
  pat: hir::PatIdx,
) -> (Pat, Ty) {
  let pat_ = match pat {
    Some(x) => x,
    None => return (Pat::zero(Con::Any, pat), Ty::None),
  };
  match &ars.pat[pat_] {
    // sml_def(32)
    hir::Pat::Wild => any(st, pat),
    // sml_def(33)
    hir::Pat::SCon(scon) => {
      let con = match *scon {
        hir::SCon::Int(i) => Con::Int(i),
        hir::SCon::Real(_) => {
          st.err(pat_, ErrorKind::RealPat);
          Con::Any
        }
        hir::SCon::Word(w) => Con::Word(w),
        hir::SCon::Char(c) => Con::Char(c),
        hir::SCon::String(ref s) => Con::String(s.clone()),
      };
      (Pat::zero(con, pat), get_scon(scon))
    }
    hir::Pat::Con(path, arg) => {
      let arg = arg.map(|x| get(st, cx, ars, ve, x));
      let env = match get_env(&cx.env, path.structures()) {
        Ok(x) => x,
        Err(name) => {
          st.err(pat_, ErrorKind::Undefined(Item::Struct, name.clone()));
          return any(st, pat);
        }
      };
      let maybe_val_info = env.val_env.get(path.last());
      let is_var = arg.is_none() && path.structures().is_empty() && ok_val_info(maybe_val_info);
      // sml_def(34)
      if is_var {
        let (pm_pat, ty) = any(st, pat);
        insert_name(st, ve, path.last().clone(), ty.clone(), pat_);
        return (pm_pat, ty);
      }
      let val_info = match maybe_val_info {
        Some(x) => x,
        None => {
          st.err(pat_, ErrorKind::Undefined(Item::Val, path.last().clone()));
          return any(st, pat);
        }
      };
      let variant_name = match &val_info.id_status {
        IdStatus::Val => {
          st.err(pat_, ErrorKind::PatValIdStatus);
          VariantName::Name(path.last().clone())
        }
        IdStatus::Con => VariantName::Name(path.last().clone()),
        IdStatus::Exn(exn) => VariantName::Exn(exn.clone()),
      };
      let ty = instantiate(st, &val_info.ty_scheme);
      // sml_def(35), sml_def(41)
      let (sym, args, ty) = match ty {
        Ty::Con(_, sym) => {
          if arg.is_some() {
            st.err(pat_, ErrorKind::ConPatMustNotHaveArg)
          }
          (sym, Vec::new(), ty)
        }
        Ty::Fn(param_ty, mut res_ty) => {
          let sym = match res_ty.as_ref() {
            Ty::Con(_, x) => *x,
            _ => unreachable!(),
          };
          let arg_pat = match arg {
            None => {
              st.err(pat_, ErrorKind::ConPatMustHaveArg);
              Pat::zero(Con::Any, pat)
            }
            Some((arg_pat, arg_ty)) => {
              unify(st, *param_ty, arg_ty, pat_);
              apply(st.subst(), &mut res_ty);
              arg_pat
            }
          };
          (sym, vec![arg_pat], *res_ty)
        }
        _ => unreachable!(),
      };
      let pat = Pat::con(Con::Variant(sym, variant_name), args, pat);
      (pat, ty)
    }
    // sml_def(36)
    hir::Pat::Record { rows, allows_other } => {
      if *allows_other {
        // sml_def(38)
        st.err(pat_, ErrorKind::Unsupported("`...` pattern rows"));
      }
      let mut labs = Vec::<hir::Lab>::with_capacity(rows.len());
      let mut pats = Vec::<Pat>::with_capacity(rows.len());
      let ty = record(st, rows, pat_, |st, lab, pat| {
        let (pm_pat, ty) = get(st, cx, ars, ve, pat);
        labs.push(lab.clone());
        pats.push(pm_pat);
        ty
      });
      (Pat::con(Con::Record(labs), pats, pat), ty)
    }
    // sml_def(42)
    hir::Pat::Typed(inner, want) => {
      let (pm_pat, got) = get(st, cx, ars, ve, *inner);
      let mut want = ty::get(st, cx, ars, *want);
      unify(st, want.clone(), got, inner.unwrap_or(pat_));
      apply(st.subst(), &mut want);
      (pm_pat, want)
    }
    // sml_def(43)
    hir::Pat::As(ref name, inner) => {
      if !ok_val_info(cx.env.val_env.get(name)) {
        st.err(pat_, ErrorKind::InvalidAsPatName(name.clone()));
      }
      let (pm_pat, ty) = get(st, cx, ars, ve, *inner);
      insert_name(st, ve, name.clone(), ty.clone(), pat_);
      (pm_pat, ty)
    }
  }
}

fn any(st: &mut St, pat: hir::PatIdx) -> (Pat, Ty) {
  (Pat::zero(Con::Any, pat), Ty::MetaVar(st.gen_meta_var()))
}

fn ok_val_info(vi: Option<&ValInfo>) -> bool {
  vi.map_or(true, |vi| matches!(vi.id_status, IdStatus::Val))
}

fn insert_name<I>(st: &mut St, ve: &mut ValEnv, name: hir::Name, ty: Ty, idx: I)
where
  I: Into<hir::Idx>,
{
  let vi = ValInfo {
    ty_scheme: TyScheme::zero(ty),
    id_status: IdStatus::Val,
  };
  if cannot_bind_val(name.as_str()) {
    st.err(idx, ErrorKind::InvalidRebindName(name));
  } else if let Some(e) = ins_no_dupe(ve, name, vi, Item::Val) {
    st.err(idx, e);
  }
}

pub(crate) fn get_match<I>(
  st: &mut St,
  pats: Vec<Pat>,
  ty: Ty,
  f: Option<fn(Vec<Pat>) -> ErrorKind>,
  marker: ErrorsMarker,
  idx: I,
) where
  I: Into<hir::Idx>,
{
  // TODO remove this
  st.did_error_since(marker);
  // NOTE: instead of take/set, this could probably be done with borrows instead. It's a little
  // annoying though because I'd need to make Lang have a lifetime parameter, which means Pat would
  // need one too, and then things get weird. Maybe the pattern_match API needs some work.
  let lang = Lang {
    syms: std::mem::take(&mut st.syms),
  };
  let ck = pattern_match::check(&lang, pats, ty);
  st.syms = lang.syms;
  let ck = match ck {
    Ok(x) => x,
    // we already should have emitted other errors in this case
    Err(_) => return,
  };
  let mut unreachable: Vec<_> = ck.unreachable.into_iter().flatten().collect();
  unreachable.sort_unstable_by_key(|x| x.into_raw());
  for un in unreachable {
    st.err(un, ErrorKind::UnreachablePattern);
  }
  if !ck.missing.is_empty() {
    if let Some(f) = f {
      st.err(idx, f(ck.missing));
    }
  }
}
