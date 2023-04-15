//! Semantic types and unification.

#![allow(clippy::module_name_repetitions)]

use crate::{data, fmt_util, overload, sym::Sym};
use std::collections::BTreeMap;

/// Storage of all the semantic types.
///
/// - TODO should this really implement Clone?
/// - TODO should we merge this with Syms?
#[derive(Debug, Clone)]
pub struct Tys {
  meta_var_rank: u16,
  pub(crate) meta_var_data: Vec<MetaTyVarData>,
  pub(crate) fixed_var_data: Vec<FixedTyVarData>,
  record: data::Map<RecordData>,
  con: data::Map<ConData>,
  fn_: data::Map<FnData>,
}

impl Default for Tys {
  fn default() -> Self {
    let mut ret = Self {
      meta_var_rank: 0,
      meta_var_data: Vec::new(),
      fixed_var_data: Vec::new(),
      record: data::Map::default(),
      con: data::Map::default(),
      fn_: data::Map::default(),
    };
    // @sync(special_sym_order)
    for (sym, ty) in [
      (Sym::EXN, Ty::EXN),
      (Sym::INT, Ty::INT),
      (Sym::WORD, Ty::WORD),
      (Sym::REAL, Ty::REAL),
      (Sym::CHAR, Ty::CHAR),
      (Sym::STRING, Ty::STRING),
      (Sym::BOOL, Ty::BOOL),
    ] {
      assert_eq!(ret.con(Vec::new(), sym), ty);
    }
    ret
  }
}

impl Tys {
  /// Returns a fresh regular meta type variable.
  pub fn meta_var(&mut self, g: Generalizable) -> Ty {
    self.meta_var_kind(g, TyVarKind::Regular)
  }

  /// Returns a fresh meta type variable with the given kind.
  pub fn meta_var_kind(&mut self, g: Generalizable, kind: TyVarKind) -> Ty {
    self.mv(g, UnsolvedMetaTyVarKind::Kind(kind))
  }

  /// Returns a fresh unresolved record meta type variable with the given rows.
  pub fn unresolved_record(&mut self, g: Generalizable, rows: RecordData, idx: sml_hir::Idx) -> Ty {
    let ur = UnresolvedRecordMetaTyVar { rows, idx };
    self.mv(g, UnsolvedMetaTyVarKind::UnresolvedRecord(ur))
  }

  fn mv(&mut self, g: Generalizable, kind: UnsolvedMetaTyVarKind) -> Ty {
    let rank = match g {
      Generalizable::Sometimes => MetaTyVarRank::Finite(self.meta_var_rank),
      Generalizable::Always => MetaTyVarRank::Infinite,
    };
    let ret = Ty { kind: TyKind::MetaVar, idx: idx::Idx::new(self.meta_var_data.len()) };
    self.meta_var_data.push(MetaTyVarData::Unsolved(UnsolvedMetaTyVarData { rank, kind }));
    ret
  }

  /// Returns a fresh fixed type variable from the HIR type variable.
  pub fn fixed_var(&mut self, ty_var: sml_hir::TyVar, src: TyVarSrc) -> Ty {
    let ret = Ty { kind: TyKind::FixedVar, idx: idx::Idx::new(self.fixed_var_data.len()) };
    self.fixed_var_data.push(FixedTyVarData { ty_var, src });
    ret
  }

  /// Returns the unique record type for the data.
  pub fn record(&mut self, data: RecordData) -> Ty {
    Ty { kind: TyKind::Record, idx: self.record.get_idx(data) }
  }

  /// Returns the unique constructor type for the data.
  pub fn con(&mut self, args: Vec<Ty>, sym: Sym) -> Ty {
    let data = ConData { args, sym };
    Ty { kind: TyKind::Con, idx: self.con.get_idx(data) }
  }

  /// Returns the unique function type for the data.
  pub fn fun(&mut self, param: Ty, res: Ty) -> Ty {
    let data = FnData { param, res };
    Ty { kind: TyKind::Fn, idx: self.fn_.get_idx(data) }
  }

  /// Returns the data for this type.
  #[must_use]
  pub fn data(&self, ty: Ty) -> TyData {
    self.canonicalize(ty).1
  }

  /// Returns the "canonical" `Ty` that represents this, and its data.
  ///
  /// For many types this will be the type passed in itself, but if e.g. `ty` was a meta var that is
  /// solved to another type, we will return that other type.
  #[must_use]
  pub fn canonicalize(&self, mut ty: Ty) -> (Ty, TyData) {
    let data = loop {
      break match ty.kind {
        TyKind::None => TyData::None,
        TyKind::BoundVar => TyData::BoundVar(BoundTyVar(ty.idx)),
        TyKind::MetaVar => match &self.meta_var_data[ty.idx.to_usize()] {
          // recur and return the data for solved meta vars.
          MetaTyVarData::Solved(new_ty) => {
            ty = *new_ty;
            continue;
          }
          MetaTyVarData::Unsolved(data) => TyData::UnsolvedMetaVar(data.clone()),
        },
        TyKind::FixedVar => TyData::FixedVar(self.fixed_var_data[ty.idx.to_usize()].clone()),
        TyKind::Record => TyData::Record(self.record.get_data(ty.idx).clone()),
        TyKind::Con => TyData::Con(self.con.get_data(ty.idx).clone()),
        TyKind::Fn => TyData::Fn(self.fn_.get_data(ty.idx).clone()),
      };
    };
    (ty, data)
  }

  /// Increases the rank for fresh meta vars.
  pub fn inc_meta_var_rank(&mut self) {
    self.meta_var_rank += 1;
  }

  /// Decreases the rank for fresh meta vars.
  pub fn dec_meta_var_rank(&mut self) {
    self.meta_var_rank -= 1;
  }

  pub(crate) fn unsolved_meta_var(&mut self, mut ty: Ty) -> &mut UnsolvedMetaTyVarData {
    assert!(matches!(ty.kind, TyKind::MetaVar));
    // wtf, borrow checker? just let me 'return' from the first loop!!
    #[allow(clippy::while_let_loop)]
    loop {
      match &self.meta_var_data[ty.idx.to_usize()] {
        MetaTyVarData::Solved(new_ty) => ty = *new_ty,
        MetaTyVarData::Unsolved(_) => break,
      }
    }
    match &mut self.meta_var_data[ty.idx.to_usize()] {
      MetaTyVarData::Solved(_) => unreachable!(),
      MetaTyVarData::Unsolved(x) => x,
    }
  }

  pub(crate) fn unsolved_meta_vars<'a, F>(&'a self, ty: Ty, f: &mut F)
  where
    F: FnMut(Ty, &'a UnsolvedMetaTyVarData),
  {
    match ty.kind {
      // interesting case
      TyKind::MetaVar => match &self.meta_var_data[ty.idx.to_usize()] {
        // recur for solved meta vars
        MetaTyVarData::Solved(new_ty) => self.unsolved_meta_vars(*new_ty, f),
        MetaTyVarData::Unsolved(data) => f(ty, data),
      },
      // trivial base cases
      TyKind::None | TyKind::BoundVar | TyKind::FixedVar => {}
      // recursive cases
      TyKind::Record => {
        for &new_ty in self.record.get_data(ty.idx).values() {
          self.unsolved_meta_vars(new_ty, f);
        }
      }
      TyKind::Con => {
        for &new_ty in &self.con.get_data(ty.idx).args {
          self.unsolved_meta_vars(new_ty, f);
        }
      }
      TyKind::Fn => {
        let data = self.fn_.get_data(ty.idx);
        self.unsolved_meta_vars(data.param, f);
        self.unsolved_meta_vars(data.res, f);
      }
    }
  }

  /// Returns in O(1) time.
  pub(crate) fn is_generalizable(&self, rank: MetaTyVarRank) -> bool {
    match rank {
      MetaTyVarRank::Finite(r) => r > self.meta_var_rank,
      MetaTyVarRank::Infinite => true,
    }
  }
}

/// A type.
///
/// Use a `Tys` to get information about this type. The `Tys` guarantees that if two `Tys` are the
/// same via `==`, they refer to the same type.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Ty {
  pub(crate) kind: TyKind,
  pub(crate) idx: idx::Idx,
}

// TODO add consts for the special tys
impl Ty {
  /// The special none "type". Things like undefined variables and invalid constructs can say they
  /// have this "type".
  pub const NONE: Self = Self { kind: TyKind::None, idx: idx::Idx::new_u32(0) };

  /// Returns a type for the bound variable.
  #[must_use]
  pub fn bound_var(bv: BoundTyVar) -> Ty {
    Ty { kind: TyKind::BoundVar, idx: bv.0 }
  }

  /// Returns a description of this type.
  #[must_use]
  pub fn desc(self) -> &'static str {
    match self.kind {
      TyKind::None => "an unknown type",
      TyKind::BoundVar => "a bound type variable",
      TyKind::MetaVar => "an unsolved type variable",
      TyKind::FixedVar => "a fixed type variable",
      TyKind::Record => "a record or tuple type",
      TyKind::Con => "a constructor type",
      TyKind::Fn => "a function type",
    }
  }
}

const _: () = assert!(std::mem::size_of::<Ty>() == 8);

/// A kind of type.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub(crate) enum TyKind {
  None,
  BoundVar,
  MetaVar,
  FixedVar,
  Record,
  Con,
  Fn,
}

/// Data about a type.
#[derive(Debug)]
#[allow(missing_docs)]
pub enum TyData {
  None,
  BoundVar(BoundTyVar),
  UnsolvedMetaVar(UnsolvedMetaTyVarData),
  FixedVar(FixedTyVarData),
  Record(RecordData),
  Con(ConData),
  Fn(FnData),
}

/// A bound type variable.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct BoundTyVar(idx::Idx);

impl BoundTyVar {
  pub(crate) fn index_into<T>(self, xs: &[T]) -> &T {
    &xs[self.0.to_usize()]
  }

  pub(crate) fn name(self, equality: bool) -> fmt_util::TyVarName {
    fmt_util::ty_var_name(equality, self.0.to_usize())
  }

  pub(crate) fn iter_for<I, T>(xs: I) -> impl Iterator<Item = (Self, T)>
  where
    I: Iterator<Item = T>,
  {
    xs.enumerate().map(|(i, x)| (Self(idx::Idx::new(i)), x))
  }

  /// Add the result of calling `f` with a new bound ty var to the binder, binding that new bound ty
  /// var.
  pub fn add_to_binder<T, F>(binder: &mut Vec<T>, f: F) -> Self
  where
    F: FnOnce(Self) -> T,
  {
    let ret = Self(idx::Idx::new(binder.len()));
    let val = f(ret);
    binder.push(val);
    ret
  }
}

/// When a meta type variable may be generalized.
#[derive(Debug, Clone, Copy)]
pub enum Generalizable {
  /// It sometimes can be.
  Sometimes,
  /// It always can be.
  Always,
}

/// Data about a meta ty var.
#[derive(Debug, Clone)]
pub(crate) enum MetaTyVarData {
  Solved(Ty),
  Unsolved(UnsolvedMetaTyVarData),
}

/// Data about an unsolved meta ty var.
#[derive(Debug, Clone)]
pub struct UnsolvedMetaTyVarData {
  pub(crate) rank: MetaTyVarRank,
  /// What kind of unsolved meta ty var this is.
  pub kind: UnsolvedMetaTyVarKind,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub(crate) enum MetaTyVarRank {
  Finite(u16),
  Infinite,
}

/// A kind of unsolved meta ty var.
#[derive(Debug, Clone)]
pub enum UnsolvedMetaTyVarKind {
  /// It has a type variable kind.
  Kind(TyVarKind),
  /// It is an unresolved record.
  UnresolvedRecord(UnresolvedRecordMetaTyVar),
}

/// An unresolved record meta ty var.
#[derive(Debug, Clone)]
pub struct UnresolvedRecordMetaTyVar {
  /// The currently solved rows.
  pub rows: RecordData,
  /// For better error reporting.
  pub idx: sml_hir::Idx,
}

/// A kind of type variable.
#[derive(Debug, Clone)]
pub enum TyVarKind {
  /// A regular type variable, like `'a`.
  Regular,
  /// An equality type variable, like `''a`.
  Equality,
  /// An overloaded type variable, like in `+` or `*`.
  Overloaded(overload::Overload),
}

/// Data about a fixed ty var.
#[derive(Debug, Clone)]
pub struct FixedTyVarData {
  /// The ty var from the source.
  pub ty_var: sml_hir::TyVar,
  /// Where this ty var was bound.
  pub src: TyVarSrc,
}

/// Where a type variable was bound.
#[derive(Debug, Clone, Copy)]
pub enum TyVarSrc {
  /// Bound at `type` or `datatype` (or `where type`).
  Ty,
  /// Bound at `val` (or `fun`).
  Val,
}

/// Data about a record. A mapping from labels to types.
pub type RecordData = BTreeMap<sml_hir::Lab, Ty>;

/// Data about a constructor type.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ConData {
  /// The arguments.
  pub args: Vec<Ty>,
  /// The symbol.
  pub sym: Sym,
}

/// Data about a function type.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct FnData {
  /// The parameter type.
  pub param: Ty,
  /// The result type.
  pub res: Ty,
}

/// Definition: `TypeScheme`, `TypeFcn`
#[derive(Debug, Clone)]
pub struct TyScheme {
  /// The bound variables in this type scheme (or function).
  pub bound_vars: BoundTyVars,
  /// The type. It may contain bound variables.
  pub ty: Ty,
}

impl TyScheme {
  /// "Zero" as in this type scheme binds zero variables.
  #[must_use]
  pub fn zero(ty: Ty) -> Self {
    Self { bound_vars: Vec::new(), ty }
  }
}

/// The bound type variables for a [`TyScheme`].
pub type BoundTyVars = Vec<TyVarKind>;
