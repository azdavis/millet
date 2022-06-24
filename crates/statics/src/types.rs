//! Types.
//!
//! Probably the single most important file in this crate. Lots of types used pervasively across
//! this crate are defined here.

use crate::fmt_util::{comma_seq, idx_to_name, ty_var_name};
use drop_bomb::DropBomb;
use fast_hash::{FxHashMap, FxHashSet};
use std::collections::BTreeMap;
use std::fmt;
use std::sync::Arc;
use uniq::{Uniq, UniqGen};

/// Definition: Type
#[derive(Debug, Clone)]
pub(crate) enum Ty {
  None,
  /// Can only appear when this Ty is wrapped in a TyScheme.
  BoundVar(BoundTyVar),
  MetaVar(MetaTyVar),
  FixedVar(FixedTyVar),
  /// Definition: RowType
  Record(RecordTy),
  /// Definition: ConsType
  ///
  /// Use `Ty::zero` if constructing a zero-argument `Con`.
  Con(Vec<Ty>, Sym),
  /// Definition: FunType
  ///
  /// Use `Ty::fun` if constructing a `Fn` from unboxed types.
  Fn(Box<Ty>, Box<Ty>),
}

impl Ty {
  /// Returns a [`Self::Con`] with 0 arguments and the given `sym`.
  pub(crate) const fn zero(sym: Sym) -> Self {
    Self::Con(Vec::new(), sym)
  }

  /// Returns a [`Self::Fn`] from `param` to `res`.
  pub(crate) fn fun(param: Self, res: Self) -> Self {
    Self::Fn(param.into(), res.into())
  }

  pub(crate) fn display<'a>(
    &'a self,
    meta_vars: &'a MetaVarNames,
    syms: &'a Syms,
  ) -> impl fmt::Display + 'a {
    TyDisplay {
      ty: self,
      bound_vars: None,
      meta_vars,
      syms,
      prec: TyPrec::Arrow,
    }
  }

  pub(crate) fn meta_var_names(&self) -> MetaVarNames {
    let mut n = 0usize;
    let mut ret = FxHashMap::<MetaTyVar, usize>::default();
    meta_vars(
      &Subst::default(),
      &mut |x| {
        ret.entry(x).or_insert_with(|| {
          let ret = n;
          n += 1;
          ret
        });
      },
      self,
    );
    MetaVarNames(ret)
  }
}

struct TyDisplay<'a> {
  ty: &'a Ty,
  bound_vars: Option<&'a BoundTyVars>,
  meta_vars: &'a MetaVarNames,
  syms: &'a Syms,
  prec: TyPrec,
}

impl<'a> TyDisplay<'a> {
  fn with(&self, ty: &'a Ty, prec: TyPrec) -> Self {
    Self {
      ty,
      bound_vars: self.bound_vars,
      meta_vars: self.meta_vars,
      syms: self.syms,
      prec,
    }
  }
}

impl<'a> fmt::Display for TyDisplay<'a> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self.ty {
      Ty::None => f.write_str("<none>")?,
      Ty::BoundVar(bv) => {
        let vars = self.bound_vars.expect("bound ty var without a BoundTyVars");
        let equality = matches!(vars.0[bv.0], Some(TyVarKind::Equality));
        for c in ty_var_name(equality, bv.0) {
          write!(f, "{c}")?;
        }
      }
      Ty::MetaVar(mv) => {
        for c in self.meta_vars.get(mv).ok_or(fmt::Error)? {
          write!(f, "{c}")?;
        }
      }
      Ty::FixedVar(fv) => fv.fmt(f)?,
      Ty::Record(rows) => {
        if rows.is_empty() {
          return f.write_str("unit");
        }
        let is_tuple = rows.len() > 1
          && rows
            .keys()
            .enumerate()
            .all(|(idx, lab)| hir::Lab::tuple(idx) == *lab);
        if is_tuple {
          let needs_parens = self.prec > TyPrec::Star;
          if needs_parens {
            f.write_str("(")?;
          }
          let mut tys = rows.values();
          let ty = tys.next().unwrap();
          self.with(ty, TyPrec::App).fmt(f)?;
          for ty in tys {
            f.write_str(" * ")?;
            self.with(ty, TyPrec::App).fmt(f)?;
          }
          if needs_parens {
            f.write_str(")")?;
          }
        } else {
          f.write_str("{ ")?;
          comma_seq(
            f,
            rows.iter().map(|(lab, ty)| RowDisplay {
              bound_vars: self.bound_vars,
              meta_vars: self.meta_vars,
              syms: self.syms,
              lab,
              ty,
            }),
          )?;
          f.write_str(" }")?;
        }
      }
      Ty::Con(args, sym) => {
        let mut args_iter = args.iter();
        if let Some(arg) = args_iter.next() {
          if args.len() == 1 {
            self.with(arg, TyPrec::App).fmt(f)?;
          } else {
            f.write_str("(")?;
            self.with(arg, TyPrec::Arrow).fmt(f)?;
            for arg in args_iter {
              f.write_str(", ")?;
              self.with(arg, TyPrec::Arrow).fmt(f)?;
            }
            f.write_str(")")?;
          }
          f.write_str(" ")?;
        }
        match self.syms.get(sym) {
          None => f.write_str("exn")?,
          Some((name, _)) => name.fmt(f)?,
        }
      }
      Ty::Fn(param, res) => {
        let needs_parens = self.prec > TyPrec::Arrow;
        if needs_parens {
          f.write_str("(")?;
        }
        self.with(param, TyPrec::Star).fmt(f)?;
        f.write_str(" -> ")?;
        self.with(res, TyPrec::Arrow).fmt(f)?;
        if needs_parens {
          f.write_str(")")?;
        }
      }
    }
    Ok(())
  }
}

pub(crate) struct MetaVarNames(FxHashMap<MetaTyVar, usize>);

impl MetaVarNames {
  pub(crate) fn get(&self, mv: &MetaTyVar) -> Option<impl Iterator<Item = char>> {
    self
      .0
      .get(mv)
      .map(|&x| std::iter::once('?').chain(idx_to_name(x)))
  }
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
enum TyPrec {
  Arrow,
  Star,
  App,
}

struct RowDisplay<'a> {
  bound_vars: Option<&'a BoundTyVars>,
  meta_vars: &'a MetaVarNames,
  syms: &'a Syms,
  lab: &'a hir::Lab,
  ty: &'a Ty,
}

impl<'a> fmt::Display for RowDisplay<'a> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    fmt::Display::fmt(self.lab, f)?;
    f.write_str(" : ")?;
    let td = TyDisplay {
      ty: self.ty,
      bound_vars: self.bound_vars,
      meta_vars: self.meta_vars,
      syms: self.syms,
      prec: TyPrec::Arrow,
    };
    fmt::Display::fmt(&td, f)
  }
}

/// Definition: TypeScheme, TypeFcn
#[derive(Debug, Clone)]
pub(crate) struct TyScheme {
  pub(crate) bound_vars: BoundTyVars,
  pub(crate) ty: Ty,
}

impl TyScheme {
  /// zero as in this type scheme binds zero variables.
  pub(crate) fn zero(ty: Ty) -> Self {
    Self {
      bound_vars: BoundTyVars::default(),
      ty,
    }
  }

  /// one as in this type scheme binds one variable.
  pub(crate) fn one<F>(f: F) -> Self
  where
    F: FnOnce(Ty) -> (Ty, Option<TyVarKind>),
  {
    let (ty, kind) = f(Ty::BoundVar(BoundTyVar(0)));
    Self {
      bound_vars: BoundTyVars(vec![kind]),
      ty,
    }
  }

  pub(crate) fn n_ary<I>(iter: I, sym: Sym) -> Self
  where
    I: Iterator<Item = Option<TyVarKind>>,
  {
    let bound_vars = BoundTyVars(iter.collect());
    let ty = Ty::Con(
      (0..bound_vars.len())
        .map(|i| Ty::BoundVar(BoundTyVar(i)))
        .collect(),
      sym,
    );
    Self { bound_vars, ty }
  }

  pub(crate) fn display<'a>(
    &'a self,
    meta_vars: &'a MetaVarNames,
    syms: &'a Syms,
  ) -> impl fmt::Display + 'a {
    TyDisplay {
      ty: &self.ty,
      bound_vars: Some(&self.bound_vars),
      meta_vars,
      syms,
      prec: TyPrec::Arrow,
    }
  }
}

#[derive(Debug, Default, Clone)]
pub(crate) struct BoundTyVars(Vec<Option<TyVarKind>>);

impl BoundTyVars {
  pub(crate) fn len(&self) -> usize {
    self.0.len()
  }

  pub(crate) fn is_empty(&self) -> bool {
    self.0.is_empty()
  }

  pub(crate) fn kinds(&self) -> impl Iterator<Item = &Option<TyVarKind>> + '_ {
    self.0.iter()
  }
}

#[derive(Debug, Clone)]
pub(crate) enum TyVarKind {
  Equality,
  Overloaded(Overload),
  Record(RecordTy),
}

#[derive(Debug, Clone, Copy)]
pub(crate) enum Overload {
  WordInt,
  RealInt,
  Num,
  NumTxt,
}

impl Overload {
  pub(crate) fn to_syms(self) -> &'static [Sym] {
    match self {
      Self::WordInt => &[Sym::WORD, Sym::INT],
      Self::RealInt => &[Sym::REAL, Sym::INT],
      Self::Num => &[Sym::WORD, Sym::REAL, Sym::INT],
      Self::NumTxt => &[Sym::WORD, Sym::REAL, Sym::INT, Sym::STRING, Sym::CHAR],
    }
  }

  pub(crate) fn unify(self, other: Self) -> Option<Self> {
    match (self, other) {
      (Self::WordInt, Self::WordInt | Self::Num | Self::NumTxt)
      | (Self::Num | Self::NumTxt, Self::WordInt) => Some(Self::WordInt),
      (Self::WordInt, Self::RealInt) | (Self::RealInt, Self::WordInt) => None,
      (Self::RealInt, Self::RealInt | Self::Num | Self::NumTxt)
      | (Self::Num | Self::NumTxt, Self::RealInt) => Some(Self::RealInt),
      (Self::Num, Self::Num | Self::NumTxt) | (Self::NumTxt, Self::Num) => Some(Self::Num),
      (Self::NumTxt, Self::NumTxt) => Some(Self::NumTxt),
    }
  }

  pub(crate) fn desc(self) -> &'static str {
    match self {
      Overload::WordInt => "word or int",
      Overload::RealInt => "real or int",
      Overload::Num => "word, real, or int",
      Overload::NumTxt => "word, real, int, string, or char",
    }
  }
}

/// Definition: TyVar
///
/// But only kind of. There's also:
///
/// - [`MetaTyVar`]
/// - [`FixedTyVar`]
/// - [`hir::TyVar`]
///
/// Basically a de Bruijn index.
#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct BoundTyVar(usize);

impl BoundTyVar {
  pub(crate) fn index_into<'a, T>(&self, xs: &'a [T]) -> &'a T {
    xs.get(self.0).unwrap()
  }
}

/// Generated, and to be solved for a real type, by the inference algorithm.
///
/// Should eventually be solved in a [`Subst`], but before that, it may be "restricted" by the
/// `Subst` without yet being fully solved to a type.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub(crate) struct MetaTyVar(Uniq);

#[derive(Debug, Default)]
pub(crate) struct MetaTyVarGen(UniqGen);

impl MetaTyVarGen {
  pub(crate) fn gen(&mut self) -> MetaTyVar {
    MetaTyVar(self.0.gen())
  }
}

/// Corresponds to a user written type variable.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub(crate) struct FixedTyVar {
  id: Uniq,
  ty_var: hir::TyVar,
}

impl fmt::Display for FixedTyVar {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    self.ty_var.fmt(f)
  }
}

#[derive(Debug, Default)]
pub(crate) struct FixedTyVarGen(UniqGen);

impl FixedTyVarGen {
  pub(crate) fn gen(&mut self, ty_var: hir::TyVar) -> FixedTyVar {
    FixedTyVar {
      id: self.0.gen(),
      ty_var,
    }
  }
}

pub(crate) type RecordTy = BTreeMap<hir::Lab, Ty>;

/// Definition: TyName
#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub(crate) struct Sym(usize);

impl fmt::Debug for Sym {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    let mut dt = f.debug_tuple("Sym");
    match self.special() {
      None => dt.field(&self.0),
      Some(x) => dt.field(&x),
    };
    dt.finish()
  }
}

macro_rules! mk_special_syms {
  ($( ($idx:expr, $mk_ty:ident, $name:ident, $str:literal), )*) => {
    impl Sym {
      $(
        pub(crate) const $name: Self = Self($idx);
      )*

      pub(crate) fn special(&self) -> Option<&'static str> {
        let s = match *self {
          $(
            Self::$name => $str,
          )*
          _ => return None,
        };
        Some(s)
      }
    }

    impl Ty {
      $(
        mk_special_syms!(@mk_ty, $mk_ty, $name, $idx);
      )*
    }
  };
  (@mk_ty, y, $name:ident, $idx:expr) => {
    pub(crate) const $name: Ty = Ty::zero(Sym::$name);
  };
  (@mk_ty, n, $name:ident, $idx:expr) => {};
}

// keep in sync with `std_basis`. weird symbols (i.e. EXN) must come first. indices must start at 0
// and go up by 1.
mk_special_syms![
  (0, y, EXN, "exn"),
  (1, y, INT, "int"),
  (2, y, WORD, "word"),
  (3, y, REAL, "real"),
  (4, y, CHAR, "char"),
  (5, y, STRING, "string"),
  (6, y, BOOL, "bool"),
  (7, n, LIST, "list"),
  (8, n, REF, "ref"),
];

impl Sym {
  /// there's only 1, and it's EXN.
  const NUM_WEIRD: usize = 1;

  /// never call this on a weird sym.
  fn idx(&self) -> usize {
    self.0 - Self::NUM_WEIRD
  }

  /// Returns whether this `Sym` was generated by a [`Syms`] after that `Syms` generated the
  /// `marker`.
  pub(crate) fn generated_after(&self, marker: &SymsMarker) -> bool {
    *self != Self::EXN && self.idx() >= marker.0
  }
}

/// Information about generated types.
///
/// Note the `Default` impl is "fake", in that it returns a totally empty `Syms`, which will lack
/// even built-in types like `int`. For a `Syms` that does have these, see
/// [`crate::std_basis::get`].
#[derive(Debug, Default, Clone)]
pub struct Syms {
  /// remember: always use Sym::idx to index
  store: Vec<(hir::Name, TyInfo)>,
  exns: Vec<(hir::Name, Option<Ty>)>,
}

impl Syms {
  pub(crate) fn start(&mut self, name: hir::Name) -> StartedSym {
    let ty_info = TyInfo {
      ty_scheme: TyScheme::zero(Ty::None),
      val_env: ValEnv::default(),
    };
    self.store.push((name, ty_info));
    StartedSym {
      bomb: DropBomb::new("must be passed to Syms::finish"),
      // calculate len after push, because we sub 1 in get, because of Sym::EXN.
      sym: Sym(self.store.len()),
    }
  }

  pub(crate) fn finish(&mut self, mut started: StartedSym, ty_info: TyInfo) {
    started.bomb.defuse();
    self.store[started.sym.idx()].1 = ty_info;
  }

  /// Returns `None` iff passed `&Sym::EXN`.
  pub(crate) fn get(&self, sym: &Sym) -> Option<(&hir::Name, &TyInfo)> {
    if *sym == Sym::EXN {
      return None;
    }
    let &(ref name, ref info) = self.store.get(sym.idx()).unwrap();
    Some((name, info))
  }

  pub(crate) fn insert_exn(&mut self, name: hir::Name, param: Option<Ty>) -> Exn {
    let ret = Exn(self.exns.len());
    self.exns.push((name, param));
    ret
  }

  pub(crate) fn get_exn(&self, exn: &Exn) -> (&hir::Name, Option<&Ty>) {
    let &(ref name, ref param) = self.exns.get(exn.0).unwrap();
    (name, param.as_ref())
  }

  pub(crate) fn mark(&self) -> SymsMarker {
    SymsMarker(self.store.len())
  }

  pub(crate) fn iter(&self) -> impl Iterator<Item = (&hir::Name, &TyInfo)> {
    self.store.iter().map(|&(ref a, ref b)| (a, b))
  }
}

/// A marker to determine when a `Sym` was generated.
pub(crate) struct SymsMarker(usize);

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub(crate) struct Exn(usize);

/// A helper to construct information about [`Syms`]s.
pub(crate) struct StartedSym {
  bomb: DropBomb,
  sym: Sym,
}

impl StartedSym {
  pub(crate) fn sym(&self) -> Sym {
    self.sym
  }
}

/// Definition: TyStr
#[derive(Debug, Clone)]
pub(crate) struct TyInfo {
  pub(crate) ty_scheme: TyScheme,
  pub(crate) val_env: ValEnv,
}

/// Definition: StrEnv
pub(crate) type StrEnv = FxHashMap<hir::Name, Env>;

/// Definition: TyEnv
pub(crate) type TyEnv = FxHashMap<hir::Name, TyInfo>;

/// Definition: ValEnv
pub(crate) type ValEnv = FxHashMap<hir::Name, ValInfo>;

#[derive(Debug, Clone)]
pub(crate) struct ValInfo {
  pub(crate) ty_scheme: TyScheme,
  pub(crate) id_status: IdStatus,
}

/// Definition: IdStatus
#[derive(Debug, Clone)]
pub(crate) enum IdStatus {
  Con,
  Exn(Exn),
  Val,
}

impl IdStatus {
  pub(crate) fn same_kind_as(&self, other: &Self) -> bool {
    matches!(
      (self, other),
      (Self::Con, Self::Con) | (Self::Exn(_), Self::Exn(_)) | (Self::Val, Self::Val)
    )
  }
}

/// Definition: Env
#[derive(Debug, Default, Clone)]
pub(crate) struct Env {
  pub(crate) str_env: StrEnv,
  pub(crate) ty_env: TyEnv,
  pub(crate) val_env: ValEnv,
}

impl Env {
  pub(crate) fn extend(&mut self, env: Env) {
    self.str_env.extend(env.str_env);
    self.ty_env.extend(env.ty_env);
    self.val_env.extend(env.val_env);
  }
}

/// Definition: Context
///
/// No need for the set of ty names as from the Definition; it seems to only be used to ensure a
/// type name does not escape its scope, and for that we use `Sym::generated_after`.
#[derive(Debug, Clone)]
pub(crate) struct Cx {
  pub(crate) env: Arc<Env>,
  /// the Definition has this as a set, but we have it as a mapping.
  pub(crate) ty_vars: FxHashMap<hir::TyVar, FixedTyVar>,
}

impl Cx {
  pub(crate) fn as_mut_env(&mut self) -> &mut Env {
    Arc::make_mut(&mut self.env)
  }
}

/// Definition: TyNameSet
pub(crate) type TyNameSet = FxHashSet<Sym>;

/// Definition: Sig
#[derive(Debug, Clone)]
pub(crate) struct Sig {
  pub(crate) ty_names: TyNameSet,
  pub(crate) env: Env,
}

/// Definition: FunSig
#[derive(Debug, Clone)]
pub(crate) struct FunSig {
  pub(crate) param: Sig,
  pub(crate) res: Sig,
}

pub(crate) type SigEnv = FxHashMap<hir::Name, Sig>;
pub(crate) type FunEnv = FxHashMap<hir::Name, FunSig>;

/// Definition: Basis
#[derive(Debug, Clone, Default)]
pub struct Bs {
  pub(crate) fun_env: FunEnv,
  pub(crate) sig_env: SigEnv,
  pub(crate) env: Arc<Env>,
}

impl Bs {
  pub(crate) fn as_cx(&self) -> Cx {
    Cx {
      env: Arc::clone(&self.env),
      ty_vars: FxHashMap::default(),
    }
  }

  pub(crate) fn as_mut_env(&mut self) -> &mut Env {
    Arc::make_mut(&mut self.env)
  }

  pub(crate) fn extend(&mut self, bs: Self) {
    self.fun_env.extend(bs.fun_env);
    self.sig_env.extend(bs.sig_env);
    let env = self.as_mut_env();
    env
      .str_env
      .extend(bs.env.str_env.iter().map(|(a, b)| (a.clone(), b.clone())));
    env
      .ty_env
      .extend(bs.env.ty_env.iter().map(|(a, b)| (a.clone(), b.clone())));
    env
      .val_env
      .extend(bs.env.val_env.iter().map(|(a, b)| (a.clone(), b.clone())));
  }
}

pub(crate) type Subst = FxHashMap<MetaTyVar, SubstEntry>;

#[derive(Debug)]
pub(crate) enum SubstEntry {
  /// We solved this meta var to a `Ty`.
  Solved(Ty),
  /// This meta var is a special type variable, into which only certain kinds of types can be
  /// substituted.
  Kind(TyVarKind),
}

#[derive(Debug, Default, Clone)]
pub(crate) struct FixedTyVars(BTreeMap<FixedTyVar, Option<BoundTyVar>>);

impl FixedTyVars {
  pub(crate) fn insert(&mut self, var: FixedTyVar) {
    assert!(self.0.insert(var, None).is_none())
  }

  /// the ordering is always the same.
  pub(crate) fn iter(&self) -> impl Iterator<Item = &FixedTyVar> + '_ {
    self.0.keys()
  }
}

/// generalizes the type in the type scheme.
///
/// replaces any fixed vars from `fixed_vars`, as well as any meta vars not already solved by
/// `subst`, in the type, with bound vars, and updates the type scheme to bind those vars.
///
/// panics if the type scheme already binds vars.
///
/// TODO remove ty_vars(cx)? what even is that?
#[must_use = "must check whether there were record meta vars"]
pub(crate) fn generalize(
  subst: &Subst,
  fixed: FixedTyVars,
  ty_scheme: &mut TyScheme,
) -> Option<HasRecordMetaVars> {
  assert!(ty_scheme.bound_vars.is_empty());
  let mut meta = FxHashMap::<MetaTyVar, Option<BoundTyVar>>::default();
  meta_vars(subst, &mut |x| drop(meta.insert(x, None)), &ty_scheme.ty);
  let mut g = Generalizer {
    subst,
    fixed,
    meta,
    bound_vars: BoundTyVars::default(),
    has_record_meta_var: false,
  };
  g.go(&mut ty_scheme.ty);
  ty_scheme.bound_vars = g.bound_vars;
  g.has_record_meta_var.then(|| HasRecordMetaVars)
}

/// a marker for when a type contained record meta vars.
pub(crate) struct HasRecordMetaVars;

/// like [`generalize`], but:
///
/// - doesn't allow meta vars
/// - always generalizes exactly the given fixed vars, even if they don't appear in the
///   `ty_scheme.ty`
pub(crate) fn generalize_fixed(mut fixed: FixedTyVars, ty_scheme: &mut TyScheme) {
  assert!(ty_scheme.bound_vars.is_empty());
  let mut bound_vars = Vec::with_capacity(fixed.0.len());
  for (idx, (fv, bv)) in fixed.0.iter_mut().enumerate() {
    assert!(bv.is_none());
    bound_vars.push(fv.ty_var.is_equality().then(|| TyVarKind::Equality));
    *bv = Some(BoundTyVar(idx));
  }
  let mut g = Generalizer {
    subst: &Subst::default(),
    fixed,
    meta: FxHashMap::default(),
    bound_vars: BoundTyVars(bound_vars),
    has_record_meta_var: false,
  };
  g.go(&mut ty_scheme.ty);
  ty_scheme.bound_vars = g.bound_vars;
  assert!(
    !g.has_record_meta_var,
    "there should be no meta vars at all, much less record ones"
  );
}

fn meta_vars<F>(subst: &Subst, f: &mut F, ty: &Ty)
where
  F: FnMut(MetaTyVar),
{
  match ty {
    Ty::None | Ty::BoundVar(_) | Ty::FixedVar(_) => {}
    Ty::MetaVar(mv) => match subst.get(mv) {
      None | Some(SubstEntry::Kind(_)) => {
        f(mv.clone());
      }
      Some(SubstEntry::Solved(ty)) => meta_vars(subst, f, ty),
    },
    Ty::Record(rows) => {
      for ty in rows.values() {
        meta_vars(subst, f, ty);
      }
    }
    Ty::Con(args, _) => {
      for ty in args.iter() {
        meta_vars(subst, f, ty);
      }
    }
    Ty::Fn(param, res) => {
      meta_vars(subst, f, param);
      meta_vars(subst, f, res);
    }
  }
}

struct Generalizer<'a> {
  subst: &'a Subst,
  fixed: FixedTyVars,
  meta: FxHashMap<MetaTyVar, Option<BoundTyVar>>,
  bound_vars: BoundTyVars,
  has_record_meta_var: bool,
}

impl<'a> Generalizer<'a> {
  fn go(&mut self, ty: &mut Ty) {
    match ty {
      Ty::None => {}
      Ty::BoundVar(_) => unreachable!("bound vars should be instantiated"),
      Ty::MetaVar(mv) => match self.subst.get(mv) {
        None => handle_bv(
          self.meta.get_mut(mv),
          &mut self.bound_vars,
          &mut self.has_record_meta_var,
          None,
          ty,
        ),
        Some(entry) => match entry {
          SubstEntry::Solved(t) => {
            *ty = t.clone();
            self.go(ty);
          }
          SubstEntry::Kind(k) => handle_bv(
            self.meta.get_mut(mv),
            &mut self.bound_vars,
            &mut self.has_record_meta_var,
            Some(k.clone()),
            ty,
          ),
        },
      },
      Ty::FixedVar(fv) => handle_bv(
        self.fixed.0.get_mut(fv),
        &mut self.bound_vars,
        &mut self.has_record_meta_var,
        fv.ty_var.is_equality().then(|| TyVarKind::Equality),
        ty,
      ),
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
  bound_vars: &mut BoundTyVars,
  has_record_meta_var: &mut bool,
  kind: Option<TyVarKind>,
  ty: &mut Ty,
) {
  let bv = match bv {
    Some(bv) => bv,
    None => return,
  };
  *ty = match bv {
    Some(bv) => Ty::BoundVar(bv.clone()),
    None => match kind {
      Some(TyVarKind::Overloaded(_)) => Ty::INT,
      Some(TyVarKind::Record(_)) => {
        *has_record_meta_var = true;
        Ty::None
      }
      None | Some(TyVarKind::Equality) => {
        let new_bv = BoundTyVar(bound_vars.len());
        *bv = Some(new_bv.clone());
        bound_vars.0.push(kind);
        Ty::BoundVar(new_bv)
      }
    },
  };
}
