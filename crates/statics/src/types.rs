//! Types.
//!
//! Probably the single most important file in this crate. Lots of types used pervasively across
//! this crate are defined here.

use crate::fmt_util::{comma_seq, ty_var_name};
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
  Record(BTreeMap<hir::Lab, Ty>),
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

  /// TODO do we need this? have it be on TyScheme, Ty, both?
  pub(crate) fn display<'a>(&'a self, syms: &'a Syms) -> impl fmt::Display + 'a {
    TyDisplay {
      ty: self,
      bound_vars: None,
      syms,
      prec: TyPrec::Arrow,
    }
  }
}

struct TyDisplay<'a> {
  ty: &'a Ty,
  /// TODO figure this out
  bound_vars: Option<&'a BoundTyVars>,
  syms: &'a Syms,
  prec: TyPrec,
}

impl<'a> TyDisplay<'a> {
  fn with(&self, ty: &'a Ty, prec: TyPrec) -> Self {
    Self {
      ty,
      bound_vars: self.bound_vars,
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
        // TODO this never gets used because we don't generalize when reporting in errors. also it
        // might have clashed with fixed vars anyway?
        let vars = self.bound_vars.expect("bound ty var without a BoundTyVars");
        let equality = matches!(vars.0[bv.0], Some(TyVarKind::Equality));
        for c in ty_var_name(equality, bv.0) {
          write!(f, "{c}")?;
        }
      }
      // TODO improve?
      Ty::MetaVar(_) => f.write_str("_")?,
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

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
enum TyPrec {
  Arrow,
  Star,
  App,
}

struct RowDisplay<'a> {
  bound_vars: Option<&'a BoundTyVars>,
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

#[derive(Debug, Clone, Copy)]
pub(crate) enum TyVarKind {
  Equality,
  Overloaded(Overload),
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
      Overload::WordInt => &[Sym::WORD, Sym::INT],
      Overload::RealInt => &[Sym::REAL, Sym::INT],
      Overload::Num => &[Sym::WORD, Sym::REAL, Sym::INT],
      Overload::NumTxt => &[Sym::WORD, Sym::REAL, Sym::INT, Sym::STRING, Sym::CHAR],
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

/// Generated, and to be substituted for a real type, by the inference algorithm.
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
  (7, y, ORDER, "order"),
  (8, n, OPTION, "option"),
  (9, n, LIST, "list"),
  (10, n, REF, "ref"),
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
/// even built-in types like `int`. For a `Syms` that does have these, see `std_basis`.
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
#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) enum IdStatus {
  Con,
  Exn(Exn),
  Val,
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

/// Definition: TyNameSet
pub(crate) type TyNameSet = FxHashSet<Sym>;

/// Definition: Sig
#[derive(Debug, Clone)]
pub(crate) struct Sig {
  /// TODO not sure
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
#[derive(Debug, Clone)]
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
}

pub(crate) type Subst = FxHashMap<MetaTyVar, SubstEntry>;

#[derive(Debug)]
pub(crate) enum SubstEntry {
  Kind(TyVarKind),
  Set(Ty),
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
pub(crate) fn generalize(subst: &Subst, fixed: FixedTyVars, ty_scheme: &mut TyScheme) {
  assert!(ty_scheme.bound_vars.is_empty());
  let mut meta = FxHashMap::<MetaTyVar, Option<BoundTyVar>>::default();
  meta_vars(subst, &mut meta, &ty_scheme.ty);
  let mut g = Generalizer {
    subst,
    fixed,
    meta,
    bound_vars: BoundTyVars::default(),
  };
  g.go(&mut ty_scheme.ty);
  ty_scheme.bound_vars = g.bound_vars;
}

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
  };
  g.go(&mut ty_scheme.ty);
  ty_scheme.bound_vars = g.bound_vars;
}

fn meta_vars(subst: &Subst, map: &mut FxHashMap<MetaTyVar, Option<BoundTyVar>>, ty: &Ty) {
  match ty {
    Ty::None | Ty::BoundVar(_) | Ty::FixedVar(_) => {}
    Ty::MetaVar(mv) => match subst.get(mv) {
      None | Some(SubstEntry::Kind(_)) => {
        map.insert(mv.clone(), None);
      }
      Some(SubstEntry::Set(ty)) => meta_vars(subst, map, ty),
    },
    Ty::Record(rows) => {
      for ty in rows.values() {
        meta_vars(subst, map, ty);
      }
    }
    Ty::Con(args, _) => {
      for ty in args.iter() {
        meta_vars(subst, map, ty);
      }
    }
    Ty::Fn(param, res) => {
      meta_vars(subst, map, param);
      meta_vars(subst, map, res);
    }
  }
}

struct Generalizer<'a> {
  subst: &'a Subst,
  fixed: FixedTyVars,
  meta: FxHashMap<MetaTyVar, Option<BoundTyVar>>,
  bound_vars: BoundTyVars,
}

impl<'a> Generalizer<'a> {
  fn go(&mut self, ty: &mut Ty) {
    match ty {
      Ty::None => {}
      Ty::BoundVar(_) => unreachable!(),
      Ty::MetaVar(mv) => match self.subst.get(mv) {
        None => {
          let bv = self.meta.get_mut(mv);
          handle_bv(bv, &mut self.bound_vars, None, ty)
        }
        Some(SubstEntry::Kind(k)) => {
          let bv = self.meta.get_mut(mv);
          handle_bv(bv, &mut self.bound_vars, Some(*k), ty)
        }
        Some(SubstEntry::Set(t)) => {
          *ty = t.clone();
          self.go(ty);
        }
      },
      Ty::FixedVar(fv) => {
        let kind = fv.ty_var.is_equality().then(|| TyVarKind::Equality);
        handle_bv(self.fixed.0.get_mut(fv), &mut self.bound_vars, kind, ty)
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
  bound_vars: &mut BoundTyVars,
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
      None | Some(TyVarKind::Equality) => {
        let new_bv = BoundTyVar(bound_vars.len());
        *bv = Some(new_bv.clone());
        bound_vars.0.push(kind);
        Ty::BoundVar(new_bv)
      }
    },
  };
}
