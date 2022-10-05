use crate::config::Cfg;
use crate::error::{ErrorKind, Item};
use crate::generalizes::eq_ty_scheme;
use crate::get_env::get_val_info;
use crate::info::{Mode, TyEntry};
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
  cfg: Cfg,
  cx: &Cx,
  ars: &sml_hir::Arenas,
  ve: &mut ValEnv,
  pat: sml_hir::PatIdx,
  g: Generalizable,
) -> (Pat, Ty) {
  let pat_ = match pat {
    Some(x) => x,
    None => return (Pat::zero(Con::Any, pat), Ty::None),
  };
  let ret = match get_(st, cfg, cx, ars, ve, pat_, g) {
    Some(x) => x,
    None => PatRet {
      pm_pat: Pat::zero(Con::Any, pat),
      ty: Ty::MetaVar(st.meta_gen.gen(g)),
      ty_scheme: None,
      def: None,
    },
  };
  let ty_entry = TyEntry { ty: ret.ty.clone(), ty_scheme: ret.ty_scheme };
  st.info().insert(pat_.into(), Some(ty_entry), ret.def);
  (ret.pm_pat, ret.ty)
}

struct PatRet {
  pm_pat: Pat,
  ty: Ty,
  ty_scheme: Option<TyScheme>,
  def: Option<Def>,
}

fn get_(
  st: &mut St,
  cfg: Cfg,
  cx: &Cx,
  ars: &sml_hir::Arenas,
  ve: &mut ValEnv,
  pat_: sml_hir::la_arena::Idx<sml_hir::Pat>,
  g: Generalizable,
) -> Option<PatRet> {
  let pat = Some(pat_);
  let mut ty_scheme = None::<TyScheme>;
  let mut def = None::<Def>;
  let (pm_pat, ty) = match &ars.pat[pat_] {
    // sml_def(32)
    sml_hir::Pat::Wild => (Pat::zero(Con::Any, pat), Ty::MetaVar(st.meta_gen.gen(g))),
    // sml_def(33)
    sml_hir::Pat::SCon(scon) => {
      let con = match scon {
        sml_hir::SCon::Int(i) => Con::Int(i.clone()),
        sml_hir::SCon::Real(_) => {
          st.err(pat_, ErrorKind::RealPat);
          Con::Any
        }
        sml_hir::SCon::Word(w) => Con::Word(*w),
        sml_hir::SCon::Char(c) => Con::Char(*c),
        sml_hir::SCon::String(s) => Con::String(s.clone()),
      };
      let t = get_scon(st, scon, g);
      (Pat::zero(con, pat), t)
    }
    sml_hir::Pat::Con(path, arg) => {
      let arg = arg.map(|x| get(st, cfg, cx, ars, ve, x, g));
      let maybe_val_info = match get_val_info(&cx.env, path) {
        Ok(x) => x,
        Err(e) => {
          st.err(pat_, e);
          return None;
        }
      };
      let is_var = arg.is_none() && path.structures().is_empty() && ok_val_info(maybe_val_info);
      // sml_def(34)
      if is_var {
        let ty = Ty::MetaVar(st.meta_gen.gen(g));
        insert_name(st, cfg, ve, path.last().clone(), ty.clone(), pat_.into());
        return Some(PatRet { pm_pat: Pat::zero(Con::Any, pat), ty, ty_scheme, def });
      }
      let val_info = match maybe_val_info {
        Some(x) => x,
        None => {
          st.err(pat_, ErrorKind::Undefined(Item::Val, path.last().clone()));
          return None;
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
              // test(pat::weird_pat_fn_1)
              _ => return None,
            },
          });
          def = val_info.def;
          let sym = match res_ty.as_ref() {
            Ty::Con(_, x) => *x,
            // test(pat::weird_pat_fn_2)
            _ => return None,
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
        _ => return None,
      };
      let pat = Pat::con(Con::Variant(sym, variant_name), args, pat);
      (pat, ty)
    }
    // sml_def(36)
    sml_hir::Pat::Record { rows, allows_other } => {
      let mut labels = BTreeSet::<sml_hir::Lab>::new();
      let mut pats = Vec::<Pat>::with_capacity(rows.len());
      let rows = record(st, rows, pat_.into(), |st, lab, pat| {
        let (pm_pat, ty) = get(st, cfg, cx, ars, ve, pat, g);
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
      let con = Con::Record { labels, allows_other: *allows_other };
      (Pat::con(con, pats, pat), ty)
    }
    // sml_def(42)
    sml_hir::Pat::Typed(inner, want) => {
      let (pm_pat, got) = get(st, cfg, cx, ars, ve, *inner, g);
      let mut want = ty::get(st, cx, ars, *want);
      unify(st, want.clone(), got, inner.unwrap_or(pat_).into());
      apply(st.subst(), &mut want);
      (pm_pat, want)
    }
    // sml_def(43)
    sml_hir::Pat::As(lhs, rhs) => {
      let (pm_pat, ty) = get(st, cfg, cx, ars, ve, *rhs, g);
      match get_as_pat_name(ars, *lhs) {
        None => st.err(lhs.unwrap_or(pat_), ErrorKind::AsPatLhsNotName),
        Some(name) => {
          if !ok_val_info(cx.env.get_val(name)) {
            st.err(pat_, ErrorKind::InvalidAsPatName(name.clone()));
          }
          insert_name(st, cfg, ve, name.clone(), ty.clone(), pat_.into());
        }
      }
      (pm_pat, ty)
    }
    sml_hir::Pat::Or(or_pat) => {
      let mut fst_ve = ValEnv::default();
      let (fst_pm_pat, mut ty) = get(st, cfg, cx, ars, &mut fst_ve, or_pat.first, g);
      let mut pm_pats = vec![fst_pm_pat];
      for &pat in or_pat.rest.iter() {
        let mut rest_ve = ValEnv::default();
        let (rest_pm_pat, rest_ty) = get(st, cfg, cx, ars, &mut rest_ve, pat, g);
        pm_pats.push(rest_pm_pat);
        let idx = sml_hir::Idx::from(pat.unwrap_or(pat_));
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
  Some(PatRet { pm_pat, ty, ty_scheme, def })
}

fn ok_val_info(vi: Option<&ValInfo>) -> bool {
  vi.map_or(true, |vi| matches!(vi.id_status, IdStatus::Val))
}

fn insert_name(
  st: &mut St,
  cfg: Cfg,
  ve: &mut ValEnv,
  name: str_util::Name,
  ty: Ty,
  idx: sml_hir::Idx,
) {
  let def = st.def(idx);
  let vi = ValInfo { ty_scheme: TyScheme::zero(ty), id_status: IdStatus::Val, def };
  match st.mode() {
    Mode::Regular(Some(_)) => {
      if cfg.mark_defined {
        st.mark_defined(idx, name.clone())
      }
    }
    Mode::Regular(None) | Mode::StdBasis(_) => {}
  }
  if let Some(e) = ins_check_name(ve, name, vi, Item::Val) {
    st.err(idx, e);
  }
}

fn get_as_pat_name(ars: &sml_hir::Arenas, pat: sml_hir::PatIdx) -> Option<&str_util::Name> {
  match &ars.pat[pat?] {
    sml_hir::Pat::Typed(pat, _) => get_as_pat_name_inner(&ars.pat[(*pat)?]),
    pat => get_as_pat_name_inner(pat),
  }
}

fn get_as_pat_name_inner(pat: &sml_hir::Pat) -> Option<&str_util::Name> {
  match pat {
    sml_hir::Pat::Con(name, None) => name.structures().is_empty().then_some(name.last()),
    _ => None,
  }
}
