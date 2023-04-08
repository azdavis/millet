//! Generalization, one of the fundamental operations on types for the inference algorithm.

use crate::ty_var::meta::{MetaTyVar, MetaTyVarGeneralizer};
use crate::types::{
  BoundTyVars, FixedTyVars, MetaTyVarKind, RecordTy, Subst, SubstEntry, Ty, TyScheme, TyVarKind,
};
use crate::{overload, ty_var::bound::BoundTyVar, util::meta_vars};
use fast_hash::FxHashMap;

/// given a type scheme that binds no vars, mutate it and the type to be generalized.
///
/// replaces:
///
/// - any fixed vars from `fixed_vars`
/// - any meta vars not already solved by `subst` which were generated after the `mv_marker`
///
/// in the type with bound vars, and updates the type scheme to bind those vars.
///
/// panics if the type scheme already binds vars.
#[allow(clippy::needless_pass_by_value)]
pub(crate) fn generalize(
  mv_generalizer: MetaTyVarGeneralizer,
  subst: &Subst,
  fixed: FixedTyVars,
  ty_scheme: &mut TyScheme,
) -> Result<(), RecordMetaVar> {
  assert!(ty_scheme.bound_vars.is_empty());
  let mut meta = FxHashMap::<MetaTyVar, Option<BoundTyVar>>::default();
  // assigning 'ranks' to meta vars is all in service of allowing `meta` to be computed efficiently.
  // if we did not, we would have to traverse the whole `Env` to know what ty vars are present in
  // it, and subtract those vars from the vars in `ty_scheme.ty`.
  meta_vars(subst, &ty_scheme.ty, &mut |mv, _| {
    if mv_generalizer.is_generalizable(mv) {
      meta.insert(mv, None);
    }
  });
  let mut g = Generalizer {
    subst,
    fixed,
    meta,
    var_state: VarState { bound_vars: BoundTyVars::default(), ret: Ok(()) },
  };
  g.go(&mut ty_scheme.ty);
  ty_scheme.bound_vars = g.var_state.bound_vars;
  g.var_state.ret
}

/// a marker for when a type contained record meta vars.
#[derive(Debug)]
pub(crate) struct RecordMetaVar {
  pub(crate) idx: sml_hir::Idx,
  pub(crate) rows: RecordTy,
}

/// like [`generalize`], but:
///
/// - doesn't allow meta vars
/// - always generalizes exactly the given fixed vars, even if they don't appear in the
///   `ty_scheme.ty`
///
/// use this to:
///
/// - explicitly create a ty scheme with the written arity, e.g. to support phantom types.
/// - preserve the order of fixed type vars for the bound ty var binders.
pub(crate) fn generalize_fixed(mut fixed: FixedTyVars, ty_scheme: &mut TyScheme) {
  assert!(ty_scheme.bound_vars.is_empty());
  let mut bound_vars = Vec::with_capacity(fixed.len());
  for (fv, bv) in &mut fixed {
    assert!(bv.is_none());
    let new_bv = BoundTyVar::add_to_binder(&mut bound_vars, |_| {
      fv.ty_var().is_equality().then_some(TyVarKind::Equality)
    });
    *bv = Some(new_bv);
  }
  let mut g = Generalizer {
    subst: &Subst::default(),
    fixed,
    meta: FxHashMap::default(),
    var_state: VarState { bound_vars, ret: Ok(()) },
  };
  g.go(&mut ty_scheme.ty);
  ty_scheme.bound_vars = g.var_state.bound_vars;
  assert!(g.var_state.ret.is_ok(), "there should be no meta vars at all, much less record ones");
}

struct Generalizer<'a> {
  subst: &'a Subst,
  fixed: FixedTyVars,
  meta: FxHashMap<MetaTyVar, Option<BoundTyVar>>,
  var_state: VarState,
}

struct VarState {
  bound_vars: BoundTyVars,
  ret: Result<(), RecordMetaVar>,
}

impl Generalizer<'_> {
  fn go(&mut self, ty: &mut Ty) {
    match ty {
      Ty::None => {}
      Ty::BoundVar(_) => unreachable!("bound vars should be instantiated"),
      Ty::MetaVar(mv) => match self.subst.get(*mv) {
        None => handle_bv(self.meta.get_mut(mv), &mut self.var_state, None, ty),
        Some(entry) => match entry {
          SubstEntry::Solved(t) => {
            *ty = t.clone();
            self.go(ty);
          }
          SubstEntry::Kind(k) => {
            handle_bv(self.meta.get_mut(mv), &mut self.var_state, Some(k.clone()), ty);
          }
        },
      },
      Ty::FixedVar(fv) => {
        let k: Option<MetaTyVarKind> =
          fv.ty_var().is_equality().then(|| TyVarKind::Equality.into());
        handle_bv(self.fixed.get_mut(fv), &mut self.var_state, k, ty);
      }
      Ty::Record(rows) => {
        for ty in rows.values_mut() {
          self.go(ty);
        }
      }
      Ty::Con(args, _) => {
        for ty in args.iter_mut() {
          self.go(ty);
        }
      }
      Ty::Fn(param, res) => {
        self.go(param);
        self.go(res);
      }
    }
  }
}

fn handle_bv(
  bv: Option<&mut Option<BoundTyVar>>,
  var_state: &mut VarState,
  kind: Option<MetaTyVarKind>,
  ty: &mut Ty,
) {
  let bv = match bv {
    Some(bv) => bv,
    None => return,
  };
  *ty = match bv {
    Some(bv) => Ty::BoundVar(*bv),
    None => match kind {
      Some(MetaTyVarKind::TyVarKind(TyVarKind::Overloaded(ov))) => match ov {
        overload::Overload::Basic(b) => match b {
          overload::Basic::Int => Ty::INT,
          overload::Basic::Real => Ty::REAL,
          overload::Basic::Word => Ty::WORD,
          overload::Basic::String => Ty::STRING,
          overload::Basic::Char => Ty::CHAR,
        },
        // all composite overloads contain, and default to, int.
        overload::Overload::Composite(_) => Ty::INT,
      },
      Some(MetaTyVarKind::Record(rows, idx)) => {
        if var_state.ret.is_ok() {
          var_state.ret = Err(RecordMetaVar { idx, rows });
        }
        Ty::None
      }
      Some(MetaTyVarKind::TyVarKind(TyVarKind::Equality)) => {
        let new_bv =
          BoundTyVar::add_to_binder(&mut var_state.bound_vars, |_| Some(TyVarKind::Equality));
        *bv = Some(new_bv);
        Ty::BoundVar(new_bv)
      }
      None => {
        let new_bv = BoundTyVar::add_to_binder(&mut var_state.bound_vars, |_| None);
        *bv = Some(new_bv);
        Ty::BoundVar(new_bv)
      }
    },
  };
}
