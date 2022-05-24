use crate::error::Error;
use crate::pat_match::{Con, Lang, Pat};
use crate::st::St;
use crate::ty;
use crate::types::{Cx, IdStatus, Ty, TyScheme, ValEnv, ValInfo};
use crate::unify::unify;
use crate::util::{apply, get_env, get_scon, instantiate, record};

pub(crate) fn get(
  st: &mut St,
  cx: &Cx,
  ars: &hir::Arenas,
  ve: &mut ValEnv,
  pat: hir::PatIdx,
) -> (Pat, Ty) {
  match &ars.pat[pat] {
    hir::Pat::None => (Pat::zero(Con::Any, pat), Ty::None),
    hir::Pat::Wild => any(st, pat),
    hir::Pat::SCon(scon) => {
      let con = match *scon {
        hir::SCon::Int(i) => Con::Int(i),
        hir::SCon::Real(_) => {
          st.err(Error::RealPat);
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
      let env = match get_env(&cx.env, path) {
        Ok(x) => x,
        Err(_) => {
          st.err(Error::Undefined);
          return any(st, pat);
        }
      };
      let maybe_val_info = env.val_env.get(path.last());
      let is_var = arg.is_none() && path.structures().is_empty() && ok_val_info(maybe_val_info);
      if is_var {
        let (pm_pat, ty) = any(st, pat);
        insert_name(st, ve, path.last().clone(), ty.clone());
        return (pm_pat, ty);
      }
      let val_info = match maybe_val_info {
        Some(x) => x,
        None => {
          st.err(Error::Undefined);
          return any(st, pat);
        }
      };
      if let IdStatus::Val = val_info.id_status {
        st.err(Error::PatValIdStatus);
      }
      let ty = instantiate(st, &val_info.ty_scheme);
      let (sym, args, ty) = match ty {
        Ty::Con(_, sym) => {
          if arg.is_some() {
            st.err(Error::PatMustNotHaveArg)
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
              st.err(Error::PatMustHaveArg);
              Pat::zero(Con::Any, pat)
            }
            Some((arg_pat, arg_ty)) => {
              unify(st, *param_ty, arg_ty);
              apply(st.subst(), &mut res_ty);
              arg_pat
            }
          };
          (sym, vec![arg_pat], *res_ty)
        }
        _ => unreachable!(),
      };
      let pat = Pat::con(Con::Variant(sym, path.last().clone()), args, pat);
      (pat, ty)
    }
    hir::Pat::Record { rows, allows_other } => {
      if *allows_other {
        st.err(Error::Unimplemented);
      }
      let mut labs = Vec::<hir::Lab>::with_capacity(rows.len());
      let mut pats = Vec::<Pat>::with_capacity(rows.len());
      let ty = record(st, rows, |st, lab, pat| {
        let (pm_pat, ty) = get(st, cx, ars, ve, pat);
        labs.push(lab.clone());
        pats.push(pm_pat);
        ty
      });
      (Pat::con(Con::Record(labs), pats, pat), ty)
    }
    hir::Pat::Typed(pat, want) => {
      let (pm_pat, got) = get(st, cx, ars, ve, *pat);
      let mut want = ty::get(st, cx, ars, *want);
      unify(st, want.clone(), got);
      apply(st.subst(), &mut want);
      (pm_pat, want)
    }
    hir::Pat::As(ref name, pat) => {
      if !ok_val_info(cx.env.val_env.get(name)) {
        st.err(Error::InvalidAsPatName);
      }
      let (pm_pat, ty) = get(st, cx, ars, ve, *pat);
      insert_name(st, ve, name.clone(), ty.clone());
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

fn insert_name(st: &mut St, ve: &mut ValEnv, name: hir::Name, ty: Ty) {
  let vi = ValInfo {
    ty_scheme: TyScheme::mono(ty),
    id_status: IdStatus::Val,
  };
  if ve.insert(name, vi).is_some() {
    st.err(Error::Redefined);
  }
}

pub(crate) fn get_match(st: &mut St, pats: Vec<Pat>, ty: Ty, f: Option<fn(Vec<Pat>) -> Error>) {
  // NOTE: instead of take/set, this could probably be done with borrows instead. It's a little
  // annoying though because I'd need to make Lang have a lifetime parameter, which means Pat would
  // need one too, and then things get weird. Maybe the pattern_match API needs some work.
  let lang = Lang {
    syms: st.take_syms(),
  };
  let ck = pattern_match::check(&lang, pats, ty);
  st.set_syms(lang.syms);
  let mut unreachable: Vec<_> = ck.unreachable.into_iter().collect();
  unreachable.sort_unstable_by_key(|x| x.into_raw());
  for un in unreachable {
    st.err(Error::UnreachablePattern(un));
  }
  if !ck.missing.is_empty() {
    if let Some(f) = f {
      st.err(f(ck.missing));
    }
  }
}
