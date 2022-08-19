use crate::error::{ErrorKind, Item};
use crate::generalizes::eq_ty_scheme;
use crate::get_env::get_val_info;
use crate::info::TyEntry;
use crate::pat_match::{Con, Pat, VariantName};
use crate::st::St;
use crate::ty;
use crate::types::{
  Cx, Def, EnvLike as _, Generalizable, IdStatus, SubstEntry, Ty, TyScheme, TyVarKind, ValEnv,
  ValInfo,
};
use crate::unify::unify;
use crate::util::{apply, get_scon, ins_check_name, instantiate, record};
use std::collections::BTreeSet;

pub(crate) fn get(
  st: &mut St,
  cx: &Cx,
  ars: &hir::Arenas,
  ve: &mut ValEnv,
  pat: hir::PatIdx,
  g: Generalizable,
) -> (Pat, Ty) {
  let pat_ = match pat {
    Some(x) => x,
    None => return (Pat::zero(Con::Any, pat), Ty::None),
  };
  let ((pat, ty), ty_scheme, def) = get_(st, cx, ars, ve, pat_, g);
  let ty_entry = TyEntry {
    ty: ty.clone(),
    ty_scheme,
  };
  st.info().insert(pat_.into(), Some(ty_entry), def);
  (pat, ty)
}

fn get_(
  st: &mut St,
  cx: &Cx,
  ars: &hir::Arenas,
  ve: &mut ValEnv,
  pat_: hir::la_arena::Idx<hir::Pat>,
  g: Generalizable,
) -> ((Pat, Ty), Option<TyScheme>, Option<Def>) {
  let pat = Some(pat_);
  let mut ty_scheme = None::<TyScheme>;
  let mut def = None::<Def>;
  let pat_ty = match &ars.pat[pat_] {
    // sml_def(32)
    hir::Pat::Wild => any(st, pat, g),
    // sml_def(33)
    hir::Pat::SCon(scon) => {
      let con = match scon {
        hir::SCon::Int(i) => Con::Int(i.clone()),
        hir::SCon::Real(_) => {
          st.err(pat_, ErrorKind::RealPat);
          Con::Any
        }
        hir::SCon::Word(w) => Con::Word(*w),
        hir::SCon::Char(c) => Con::Char(*c),
        hir::SCon::String(s) => Con::String(s.clone()),
      };
      let t = get_scon(st, scon, g);
      (Pat::zero(con, pat), t)
    }
    hir::Pat::Con(path, arg) => {
      let arg = arg.map(|x| get(st, cx, ars, ve, x, g));
      let maybe_val_info = match get_val_info(&cx.env, path) {
        Ok(x) => x,
        Err(e) => {
          st.err(pat_, e);
          return (any(st, pat, g), ty_scheme, def);
        }
      };
      let is_var = arg.is_none() && path.structures().is_empty() && ok_val_info(maybe_val_info);
      // sml_def(34)
      if is_var {
        let (pm_pat, ty) = any(st, pat, g);
        insert_name(st, ve, path.last().clone(), ty.clone(), pat_.into());
        return ((pm_pat, ty), ty_scheme, def);
      }
      let val_info = match maybe_val_info {
        Some(x) => x,
        None => {
          st.err(pat_, ErrorKind::Undefined(Item::Val, path.last().clone()));
          return (any(st, pat, g), ty_scheme, def);
        }
      };
      let variant_name = match &val_info.id_status {
        IdStatus::Val => {
          st.err(pat_, ErrorKind::PatValIdStatus);
          VariantName::Name(path.last().clone())
        }
        IdStatus::Con => VariantName::Name(path.last().clone()),
        IdStatus::Exn(exn) => VariantName::Exn(*exn),
      };
      let ty = instantiate(st, val_info.ty_scheme.clone(), g);
      // sml_def(35), sml_def(41)
      let (sym, args, ty) = match ty {
        Ty::Con(_, sym) => {
          ty_scheme = Some(val_info.ty_scheme.clone());
          def = val_info.def;
          if arg.is_some() {
            st.err(pat_, ErrorKind::ConPatMustNotHaveArg)
          }
          (sym, Vec::new(), ty)
        }
        Ty::Fn(param_ty, mut res_ty) => {
          ty_scheme = Some(TyScheme {
            bound_vars: val_info.ty_scheme.bound_vars.clone(),
            ty: match &val_info.ty_scheme.ty {
              Ty::Fn(_, res_ty) => res_ty.as_ref().clone(),
              _ => unreachable!("we are in the Fn case for the ty scheme's ty"),
            },
          });
          def = val_info.def;
          let sym = match res_ty.as_ref() {
            Ty::Con(_, x) => *x,
            _ => unreachable!("a fn ctor returns the type it constructs, which will be a Con"),
          };
          let arg_pat = match arg {
            None => {
              st.err(pat_, ErrorKind::ConPatMustHaveArg);
              Pat::zero(Con::Any, pat)
            }
            Some((arg_pat, arg_ty)) => {
              unify(st, *param_ty, arg_ty, pat_.into());
              apply(st.subst(), &mut res_ty);
              arg_pat
            }
          };
          (sym, vec![arg_pat], *res_ty)
        }
        // should have already errored
        _ => return (any(st, pat, g), ty_scheme, def),
      };
      let pat = Pat::con(Con::Variant(sym, variant_name), args, pat);
      (pat, ty)
    }
    // sml_def(36)
    hir::Pat::Record { rows, allows_other } => {
      let mut labels = BTreeSet::<hir::Lab>::new();
      let mut pats = Vec::<Pat>::with_capacity(rows.len());
      let rows = record(st, rows, pat_.into(), |st, lab, pat| {
        let (pm_pat, ty) = get(st, cx, ars, ve, pat, g);
        labels.insert(lab.clone());
        pats.push(pm_pat);
        ty
      });
      let ty = if *allows_other {
        // sml_def(38)
        let mv = st.meta_gen.gen(g);
        let k = SubstEntry::Kind(TyVarKind::Record(rows));
        assert!(st.subst().insert(mv, k).is_none(),);
        Ty::MetaVar(mv)
      } else {
        Ty::Record(rows)
      };
      let con = Con::Record {
        labels,
        allows_other: *allows_other,
      };
      (Pat::con(con, pats, pat), ty)
    }
    // sml_def(42)
    hir::Pat::Typed(inner, want) => {
      let (pm_pat, got) = get(st, cx, ars, ve, *inner, g);
      let mut want = ty::get(st, cx, ars, *want);
      unify(st, want.clone(), got, inner.unwrap_or(pat_).into());
      apply(st.subst(), &mut want);
      (pm_pat, want)
    }
    // sml_def(43)
    hir::Pat::As(ref name, inner) => {
      if !ok_val_info(cx.env.get_val(name)) {
        st.err(pat_, ErrorKind::InvalidAsPatName(name.clone()));
      }
      let (pm_pat, ty) = get(st, cx, ars, ve, *inner, g);
      insert_name(st, ve, name.clone(), ty.clone(), pat_.into());
      (pm_pat, ty)
    }
    hir::Pat::Or(or_pat) => {
      let mut fst_ve = ValEnv::default();
      let (fst_pm_pat, mut ty) = get(st, cx, ars, &mut fst_ve, or_pat.first, g);
      let mut pm_pats = vec![fst_pm_pat];
      for &pat in or_pat.rest.iter() {
        let mut rest_ve = ValEnv::default();
        let (rest_pm_pat, rest_ty) = get(st, cx, ars, &mut rest_ve, pat, g);
        pm_pats.push(rest_pm_pat);
        let idx = hir::Idx::from(pat.unwrap_or(pat_));
        unify(st, ty.clone(), rest_ty, idx);
        apply(st.subst(), &mut ty);
        for (name, fst_val_info) in fst_ve.iter() {
          let rest_val_info = match rest_ve.remove(name) {
            Some(x) => x,
            None => {
              st.err(idx, ErrorKind::OrPatNotSameBindings(name.clone()));
              continue;
            }
          };
          assert!(fst_val_info.id_status.same_kind_as(&IdStatus::Val));
          assert!(rest_val_info.id_status.same_kind_as(&IdStatus::Val));
          let fst_ty_scheme = fst_val_info.ty_scheme.clone();
          let rest_ty_scheme = rest_val_info.ty_scheme.clone();
          eq_ty_scheme(st, fst_ty_scheme, rest_ty_scheme, idx);
        }
        if let Some(name) = rest_ve.into_keys().next() {
          st.err(idx, ErrorKind::OrPatNotSameBindings(name));
        }
      }
      ve.extend(fst_ve);
      (Pat::or(pm_pats, pat), ty)
    }
  };
  (pat_ty, ty_scheme, def)
}

fn any(st: &mut St, pat: hir::PatIdx, g: Generalizable) -> (Pat, Ty) {
  let p = Pat::zero(Con::Any, pat);
  let t = Ty::MetaVar(st.meta_gen.gen(g));
  (p, t)
}

fn ok_val_info(vi: Option<&ValInfo>) -> bool {
  vi.map_or(true, |vi| matches!(vi.id_status, IdStatus::Val))
}

fn insert_name(st: &mut St, ve: &mut ValEnv, name: hir::Name, ty: Ty, idx: hir::Idx) {
  let vi = ValInfo {
    ty_scheme: TyScheme::zero(ty),
    id_status: IdStatus::Val,
    def: st.def(idx),
  };
  if let Some(e) = ins_check_name(ve, name, vi, Item::Val) {
    st.err(idx, e);
  }
}
