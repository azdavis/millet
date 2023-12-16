//! Checking patterns.

use crate::info::TyEntry;
use crate::pat_match::{Con, Pat, VariantName};
use crate::util::{ins_check_name, record};
use crate::{
  compatible::eq_ty_scheme, config, error::ErrorKind, get_env::get_val_info, st::St, ty,
  unify::unify,
};
use sml_statics_types::info::{IdStatus, ValEnv, ValInfo};
use sml_statics_types::ty::{Generalizable, Ty, TyData, TyScheme};
use sml_statics_types::util::{get_scon, instantiate};
use sml_statics_types::{def, env::Cx, item::Item, mode::Mode, sym::Sym};
use std::collections::BTreeSet;

#[derive(Debug, Clone, Copy)]
pub(crate) struct Cfg {
  pub(crate) cfg: config::Cfg,
  pub(crate) gen: Generalizable,
  pub(crate) rec: bool,
}

pub(crate) fn get(
  st: &mut St<'_>,
  cfg: Cfg,
  ars: &sml_hir::Arenas,
  cx: &Cx,
  ve: &mut ValEnv,
  pat: sml_hir::PatIdx,
) -> (Pat, Ty) {
  let Some(pat_) = pat else { return (Pat::zero(Con::Any, pat), Ty::NONE) };
  let ret = match get_(st, cfg, ars, cx, ve, pat_) {
    Some(x) => x,
    None => PatRet {
      pm_pat: Pat::zero(Con::Any, pat),
      ty: st.syms_tys.tys.meta_var(cfg.gen),
      ty_scheme: None,
      defs: def::Set::default(),
    },
  };
  st.info.entries.tys.pat.insert(pat_, TyEntry::new(ret.ty, ret.ty_scheme));
  if !ret.defs.is_empty() {
    st.info.entries.defs.pat.insert(pat_, ret.defs);
  }
  (ret.pm_pat, ret.ty)
}

struct PatRet {
  pm_pat: Pat,
  ty: Ty,
  ty_scheme: Option<TyScheme>,
  defs: def::Set,
}

fn get_(
  st: &mut St<'_>,
  cfg: Cfg,
  ars: &sml_hir::Arenas,
  cx: &Cx,
  ve: &mut ValEnv,
  pat_idx: sml_hir::la_arena::Idx<sml_hir::Pat>,
) -> Option<PatRet> {
  let pat = Some(pat_idx);
  let mut ty_scheme = None::<TyScheme>;
  let mut defs = def::Set::default();
  let (pm_pat, ty) = match &ars.pat[pat_idx] {
    // @def(32)
    sml_hir::Pat::Wild => (Pat::zero(Con::Any, pat), st.syms_tys.tys.meta_var(cfg.gen)),
    // @def(33)
    sml_hir::Pat::SCon(special_con) => {
      let con = match special_con {
        sml_hir::SCon::Int(i) => Con::Int(i.clone()),
        sml_hir::SCon::Real(_) => {
          st.err(pat_idx, ErrorKind::RealPat);
          Con::Any
        }
        sml_hir::SCon::Word(w) => Con::Word(*w),
        sml_hir::SCon::Char(c) => Con::Char(*c),
        sml_hir::SCon::String(s) => Con::String(s.clone()),
      };
      let t = get_scon(&mut st.syms_tys.tys, cfg.gen, special_con);
      (Pat::zero(con, pat), t)
    }
    sml_hir::Pat::Con(path, argument) => {
      let argument = argument.map(|x| get(st, cfg, ars, cx, ve, x));
      let val_info = get_val_info(&cx.env, path);
      for e in val_info.disallow {
        st.err(pat_idx, e.into());
      }
      cov_mark::hit("rebind_ctor");
      let is_var = argument.is_none()
        && path.prefix().is_empty()
        && (val_info_for_var(val_info.val.as_ref().ok().copied()) || cfg.rec);
      // @def(34)
      if is_var {
        let ty = st.syms_tys.tys.meta_var(cfg.gen);
        insert_name(st, pat_idx.into(), cfg.cfg, ve, path.last().clone(), ty);
        // a little WET with val_info_for_var
        if let Mode::Dynamics = st.info.mode {
          assert!(st.pat_id_statuses.insert(pat_idx, IdStatus::Val).is_none());
        }
        return Some(PatRet { pm_pat: Pat::zero(Con::Any, pat), ty, ty_scheme, defs });
      }
      let val_info = match val_info.val {
        Ok(x) => x,
        Err(e) => {
          st.err(pat_idx, e.into());
          return None;
        }
      };
      if let Some(d) = &val_info.disallow {
        st.err(pat_idx, ErrorKind::Disallowed(Item::Val, d.clone(), path.last().clone()));
      }
      if let Mode::Dynamics = st.info.mode {
        assert!(st.pat_id_statuses.insert(pat_idx, val_info.id_status).is_none());
      }
      let variant_name = match &val_info.id_status {
        IdStatus::Val => {
          st.err(pat_idx, ErrorKind::PatValIdStatus);
          VariantName::Name(path.last().clone())
        }
        IdStatus::Con => VariantName::Name(path.last().clone()),
        IdStatus::Exn(exn) => VariantName::Exn(*exn),
      };
      let ty = instantiate(&mut st.syms_tys.tys, cfg.gen, &val_info.ty_scheme);
      // @def(35), @def(41)
      let (sym, arguments, ty) = match st.syms_tys.tys.data(ty) {
        TyData::Con(data) => {
          ty_scheme = Some(val_info.ty_scheme.clone());
          defs.extend(val_info.defs.iter().copied());
          if argument.is_some() {
            st.err(pat_idx, ErrorKind::ConPatMustNotHaveArg);
          }
          (data.sym, Vec::new(), ty)
        }
        TyData::Fn(data) => {
          ty_scheme = Some(TyScheme {
            bound_vars: val_info.ty_scheme.bound_vars.clone(),
            ty: match st.syms_tys.tys.data(val_info.ty_scheme.ty) {
              TyData::Fn(data) => data.res,
              _ => {
                // NOTE: @test(pat::weird_pat_fn_1) was supposed to exercise this code path, but
                // after adding `cov_mark` it became clear that the test does not actually exercise
                // this code path. changing this `return None` to an unreachable!() currently passes
                // all tests. would be good to find and add a cov mark for a test that actually hits
                // this path.
                return None;
              }
            },
          });
          defs.extend(val_info.defs.iter().copied());
          let sym = match st.syms_tys.tys.data(data.res) {
            TyData::Con(data) => data.sym,
            _ => {
              cov_mark::hit("weird_pat_fn_2");
              return None;
            }
          };
          let arg_pat = match argument {
            None => {
              st.err(pat_idx, ErrorKind::ConPatMustHaveArg);
              Pat::zero(Con::Any, pat)
            }
            Some((arg_pat, arg_ty)) => {
              unify(st, pat_idx.into(), data.param, arg_ty);
              arg_pat
            }
          };
          (sym, vec![arg_pat], data.res)
        }
        // should have already errored
        _ => return None,
      };
      let pat = Pat::con(Con::Variant(sym, variant_name), arguments, pat);
      (pat, ty)
    }
    // @def(36)
    sml_hir::Pat::Record { rows, allows_other } => {
      let mut labels = BTreeSet::<sml_hir::Lab>::new();
      let mut pats = Vec::<Pat>::with_capacity(rows.len());
      let rows = record(st, pat_idx.into(), rows, |st, lab, pat| {
        let (pm_pat, ty) = get(st, cfg, ars, cx, ve, pat);
        labels.insert(lab.clone());
        pats.push(pm_pat);
        ty
      });
      let ty = if *allows_other {
        // @def(38)
        st.syms_tys.tys.unresolved_record(cfg.gen, rows, pat_idx.into())
      } else {
        st.syms_tys.tys.record(rows)
      };
      let con = Con::Record { labels, allows_other: *allows_other };
      (Pat::con(con, pats, pat), ty)
    }
    // @def(42)
    sml_hir::Pat::Typed(inner, want) => {
      let (pm_pat, got) = get(st, cfg, ars, cx, ve, *inner);
      let want = ty::get(st, cx, ars, ty::Mode::Regular, *want);
      unify(st, inner.unwrap_or(pat_idx).into(), want, got);
      (pm_pat, want)
    }
    // @def(43)
    sml_hir::Pat::As(name, pat) => {
      let (pm_pat, ty) = get(st, cfg, ars, cx, ve, *pat);
      if !val_info_for_var(cx.env.val_env.get(name)) {
        st.err(pat_idx, ErrorKind::InvalidAsPatName(name.clone()));
      }
      insert_name(st, pat_idx.into(), cfg.cfg, ve, name.clone(), ty);
      (pm_pat, ty)
    }
    sml_hir::Pat::Or(or_pat) => {
      let mut fst_ve = ValEnv::default();
      let (fst_pm_pat, ty) = get(st, cfg, ars, cx, &mut fst_ve, or_pat.first);
      let mut pm_pats = vec![fst_pm_pat];
      for &pat in &or_pat.rest {
        let mut rest_ve = ValEnv::default();
        let (rest_pm_pat, rest_ty) = get(st, cfg, ars, cx, &mut rest_ve, pat);
        pm_pats.push(rest_pm_pat);
        let idx = sml_hir::Idx::from(pat.unwrap_or(pat_idx));
        unify(st, idx, ty, rest_ty);
        for (name, fst_val_info) in fst_ve.iter_mut() {
          let Some(rest_val_info) = rest_ve.remove(name) else {
            st.err(idx, ErrorKind::OrPatNotSameBindings(name.clone()));
            continue;
          };
          assert!(fst_val_info.id_status.same_kind_as(IdStatus::Val));
          assert!(rest_val_info.id_status.same_kind_as(IdStatus::Val));
          fst_val_info.defs.extend(rest_val_info.defs);
          let rest_ty_scheme = rest_val_info.ty_scheme.clone();
          match eq_ty_scheme(st, &fst_val_info.ty_scheme, &rest_ty_scheme) {
            Ok(()) => {}
            Err(e) => st.err(idx, e),
          }
        }
        if let Some((name, _)) = rest_ve.into_iter().next() {
          st.err(idx, ErrorKind::OrPatNotSameBindings(name.clone()));
        }
      }
      ve.append(&mut fst_ve);
      (Pat::or(pm_pats, pat), ty)
    }
    // Successor ML extension
    sml_hir::Pat::Vector(pats) => {
      let ty = st.syms_tys.tys.meta_var(Generalizable::Always);
      let iter = pats.iter().map(|&inner| {
        let (pm_pat, inner_ty) = get(st, cfg, ars, cx, ve, inner);
        unify(st, inner.unwrap_or(pat_idx).into(), ty, inner_ty);
        pm_pat
      });
      let pm_args: Vec<_> = iter.collect();
      let ty = st.syms_tys.tys.con(vec![ty], Sym::VECTOR);
      (Pat::con(Con::Vector(pm_args.len()), pm_args, pat), ty)
    }
  };
  Some(PatRet { pm_pat, ty, ty_scheme, defs })
}

pub(crate) fn val_info_for_var(vi: Option<&ValInfo>) -> bool {
  vi.map_or(true, |vi| matches!(vi.id_status, IdStatus::Val))
}

fn insert_name(
  st: &mut St<'_>,
  idx: sml_hir::Idx,
  cfg: config::Cfg,
  ve: &mut ValEnv,
  name: str_util::Name,
  ty: Ty,
) {
  let vi = ValInfo {
    ty_scheme: TyScheme::zero(ty),
    id_status: IdStatus::Val,
    defs: st.def(idx).into_iter().collect(),
    disallow: None,
  };
  match st.info.mode {
    Mode::Regular(Some(_)) => {
      if cfg.mark_defined {
        st.mark_defined(idx, name.clone());
      }
    }
    Mode::Regular(None) | Mode::BuiltinLib(_) | Mode::PathOrder | Mode::Dynamics => {}
  }
  if let Some(e) = ins_check_name(ve, name, vi, Item::Val) {
    st.err(idx, e);
  }
}

pub(crate) fn maybe_refutable(ars: &sml_hir::Arenas, pat: sml_hir::PatIdx) -> bool {
  let Some(pat) = pat else { return true };
  match &ars.pat[pat] {
    sml_hir::Pat::Wild => false,
    sml_hir::Pat::SCon(_) | sml_hir::Pat::Con(_, _) => true,
    sml_hir::Pat::Record { rows, .. } => rows.iter().any(|&(_, pat)| maybe_refutable(ars, pat)),
    sml_hir::Pat::Typed(pat, _) | sml_hir::Pat::As(_, pat) => maybe_refutable(ars, *pat),
    sml_hir::Pat::Or(or_pat) => or_pat.all_pats().any(|pat| maybe_refutable(ars, pat)),
    sml_hir::Pat::Vector(pats) => pats.iter().any(|&pat| maybe_refutable(ars, pat)),
  }
}
