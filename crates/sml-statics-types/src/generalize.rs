//! Generalization, one of the fundamental operations on types for the inference algorithm.

use crate::overload;
use crate::ty::{
  BoundTyVar, BoundTyVars, RecordData, Ty, TyData, TyKind, TyScheme, TyVarKind, Tys,
  UnresolvedRecordMetaTyVar, UnsolvedMetaTyVarKind,
};
use fast_hash::FxHashMap;
use std::collections::BTreeMap;

pub(crate) type Result<T, E = UnresolvedRecordMetaTyVar> = std::result::Result<T, E>;

/// A sequence of fixed type variables, e.g. the `('foo, 'bar)` in `val ('foo, 'bar) quz = ...`
#[derive(Debug, Default, Clone)]
pub struct FixedTyVars(BTreeMap<idx::Idx, Option<BoundTyVar>>);

impl FixedTyVars {
  /// Pushes the fixed ty var to this.
  ///
  /// # Panics
  ///
  /// If this is not a fixed ty var or if it was already inserted.
  pub fn push(&mut self, fv: Ty) {
    assert!(matches!(fv.kind, TyKind::FixedVar));
    assert!(self.0.insert(fv.idx, None).is_none());
  }

  /// Iterates over the fixed ty vars.
  pub fn iter(&self) -> impl Iterator<Item = Ty> + '_ {
    self.0.iter().map(|(&idx, _)| Ty { kind: TyKind::FixedVar, idx })
  }
}

/// Generalizes a type into a type scheme, by replacing (any fixed vars from `fixed`) and (any meta
/// vars not already solved) that appear in the type with bound vars.
///
/// # Errors
///
/// If we couldn't generalize because there was an unresolved record meta ty var.
pub fn generalize(tys: &mut Tys, fixed: FixedTyVars, ty: Ty) -> Result<TyScheme> {
  let mut meta = FxHashMap::<idx::Idx, Option<BoundTyVar>>::default();
  // assigning 'ranks' to meta vars is all in service of allowing `meta` to be computed efficiently.
  // if we did not, we would have to traverse a whole `Env` to know what ty vars are present in it,
  // and subtract those vars from the vars in `ty`.
  tys.unsolved_meta_vars(ty, &mut |mv, data| {
    if tys.is_generalizable(data.rank) {
      meta.insert(mv.idx, None);
    }
  });
  let mut st = St { fixed, meta, bound: Vec::new(), tys };
  let ty = go(&mut st, ty)?;
  Ok(TyScheme { bound_vars: st.bound, ty })
}

/// Generalizes a type **without meta vars** into a type scheme, by replacing any fixed vars from
/// `fixed` in the type with bound vars. Note that the returned type scheme always quantifies
/// **exactly** the vars from `fixed`, even if they do not appear in the type.
///
/// Use this to:
///
/// - Explicitly create a ty scheme with the written arity, e.g. to support phantom types.
/// - Preserve the order of fixed type vars for the bound ty var binders.
///
/// # Panics
///
/// If it has a bug.
#[allow(clippy::module_name_repetitions)]
pub fn generalize_fixed(tys: &mut Tys, mut fixed: FixedTyVars, ty: Ty) -> TyScheme {
  let mut bound = Vec::with_capacity(fixed.0.len());
  for (&fv, bv) in &mut fixed.0 {
    assert!(bv.is_none());
    let k = if tys.fixed_var_data[fv.to_usize()].ty_var.is_equality() {
      TyVarKind::Equality
    } else {
      TyVarKind::Regular
    };
    let new_bv = BoundTyVar::add_to_binder(&mut bound, |_| k);
    *bv = Some(new_bv);
  }
  let mut st = St { fixed, meta: FxHashMap::default(), bound, tys };
  let ty = go(&mut st, ty).expect("there should be no meta vars at all, much less record ones");
  TyScheme { bound_vars: st.bound, ty }
}

struct St<'a> {
  fixed: FixedTyVars,
  meta: FxHashMap<idx::Idx, Option<BoundTyVar>>,
  bound: BoundTyVars,
  tys: &'a mut Tys,
}

fn go(st: &mut St<'_>, ty: Ty) -> Result<Ty> {
  let (ty, data) = st.tys.canonicalize(ty);
  match data {
    // interesting cases
    TyData::UnsolvedMetaVar(umv) => {
      let bv = st.meta.get_mut(&ty.idx);
      go_bv(bv, &mut st.bound, umv.kind, ty)
    }
    TyData::FixedVar(fv) => {
      let k = if fv.ty_var.is_equality() { TyVarKind::Equality } else { TyVarKind::Regular };
      let k = UnsolvedMetaTyVarKind::Kind(k);
      let bv = st.fixed.0.get_mut(&ty.idx);
      go_bv(bv, &mut st.bound, k, ty)
    }
    // trivial base cases
    TyData::BoundVar(_) => unreachable!("bound vars should be instantiated"),
    TyData::None => Ok(Ty::NONE),
    // recursive cases
    TyData::Record(rows) => {
      let rows =
        rows.into_iter().map(|(lab, ty)| Ok((lab, go(st, ty)?))).collect::<Result<RecordData>>()?;
      Ok(st.tys.record(rows))
    }
    TyData::Con(data) => {
      let args = data.args.into_iter().map(|ty| go(st, ty)).collect::<Result<Vec<_>>>()?;
      Ok(st.tys.con(args, data.sym))
    }
    TyData::Fn(data) => {
      let param = go(st, data.param)?;
      let res = go(st, data.res)?;
      Ok(st.tys.fun(param, res))
    }
  }
}

fn go_bv(
  bv: Option<&mut Option<BoundTyVar>>,
  bound: &mut BoundTyVars,
  kind: UnsolvedMetaTyVarKind,
  ty: Ty,
) -> Result<Ty> {
  let bv = match bv {
    Some(bv) => bv,
    None => return Ok(ty),
  };
  match bv {
    Some(bv) => return Ok(Ty::bound_var(*bv)),
    None => {}
  }
  match kind {
    UnsolvedMetaTyVarKind::Kind(kind) => match kind {
      TyVarKind::Regular | TyVarKind::Equality => {
        let new_bv = BoundTyVar::add_to_binder(bound, |_| kind);
        *bv = Some(new_bv);
        Ok(Ty::bound_var(new_bv))
      }
      TyVarKind::Overloaded(ov) => match ov {
        overload::Overload::Basic(b) => match b {
          overload::Basic::Int => Ok(Ty::INT),
          overload::Basic::Real => Ok(Ty::REAL),
          overload::Basic::Word => Ok(Ty::WORD),
          overload::Basic::String => Ok(Ty::STRING),
          overload::Basic::Char => Ok(Ty::CHAR),
        },
        // all composite overloads contain, and default to, int.
        overload::Overload::Composite(_) => Ok(Ty::INT),
      },
    },
    // it is a user error if these haven't been solved by now.
    UnsolvedMetaTyVarKind::UnresolvedRecord(ur_mv) => Err(ur_mv),
  }
}
