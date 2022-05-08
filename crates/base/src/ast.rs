//! Abstract syntax trees.

use crate::loc::{Loc, Located};
use intern::StrRef;
use token::TyVar;

/// An expression.
#[derive(Debug)]
#[allow(missing_docs)]
pub enum Exp<I> {
  // begin special constants
  DecInt(i32),
  HexInt(i32),
  DecWord(i32),
  HexWord(i32),
  Real(f64),
  String(StrRef),
  Char(u8),
  // end special constants
  LongVid(Long<I>),
  Record(Vec<Row<Located<Exp<I>>>>),
  Select(Located<Label>),
  /// requires vec.len() != 1
  Tuple(Vec<Located<Exp<I>>>),
  List(Vec<Located<Exp<I>>>),
  /// requires vec.len() >= 2
  Sequence(Vec<Located<Exp<I>>>),
  /// requires !vec.is_empty()
  Let(Located<Dec<I>>, Vec<Located<Exp<I>>>),
  App(Box<Located<Exp<I>>>, Box<Located<Exp<I>>>),
  InfixApp(Box<Located<Exp<I>>>, Located<I>, Box<Located<Exp<I>>>),
  Typed(Box<Located<Exp<I>>>, Located<Ty<I>>),
  Andalso(Box<Located<Exp<I>>>, Box<Located<Exp<I>>>),
  Orelse(Box<Located<Exp<I>>>, Box<Located<Exp<I>>>),
  Handle(Box<Located<Exp<I>>>, Cases<I>),
  Raise(Box<Located<Exp<I>>>),
  If(
    Box<Located<Exp<I>>>,
    Box<Located<Exp<I>>>,
    Box<Located<Exp<I>>>,
  ),
  While(Box<Located<Exp<I>>>, Box<Located<Exp<I>>>),
  Case(Box<Located<Exp<I>>>, Cases<I>),
  Fn(Cases<I>),
}

/// A long identifier. Can have zero or more structures qualifying the identifier.
#[derive(Debug)]
pub struct Long<I> {
  /// The structures qualifying this identifier, in order.
  pub structures: Vec<Located<I>>,
  /// The final component of the identifier, after all of the zero or more structures
  pub last: Located<I>,
}

impl<I> Long<I> {
  /// Returns the location of this Long.
  pub fn loc(&self) -> Loc {
    match self.structures.first() {
      None => self.last.loc,
      Some(x) => x.loc.span(self.last.loc),
    }
  }
}

/// A row, in a record. Used for expression, pattern, and type rows.
#[derive(Debug)]
pub struct Row<T> {
  /// The label.
  pub lab: Located<Label>,
  /// The value.
  pub val: T,
}

/// A label, as in a row. See StrRef for a discussion on PartialOrd + Ord.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Label {
  /// A numeric label.
  Num(u32),
  /// A string label.
  Vid(StrRef),
}

impl Label {
  /// Returns the `idx`th tuple label, where `idx` is 0-indexed.
  pub fn tuple(idx: usize) -> Self {
    Label::Num((idx + 1).try_into().unwrap())
  }
}

/// called C 'match' in the Definition. We call it 'cases' to avoid conflicts with the Rust keyword.
#[derive(Debug)]
pub struct Cases<I> {
  /// requires !arms.is_empty()
  pub arms: Vec<Arm<I>>,
}

/// An arm of a match expression (we call it "Cases").
#[derive(Debug)]
pub struct Arm<I> {
  /// The pattern.
  pub pat: Located<Pat<I>>,
  /// The expression.
  pub exp: Located<Exp<I>>,
}

/// A declaration.
#[derive(Debug)]
#[allow(missing_docs)]
pub enum Dec<I> {
  /// requires !val_binds.is_empty()
  Val(Vec<Located<TyVar<I>>>, Vec<ValBind<I>>),
  /// requires !fval_binds.is_empty()
  Fun(Vec<Located<TyVar<I>>>, Vec<FValBind<I>>),
  /// requires !ty_binds.is_empty()
  Type(Vec<TyBind<I>>),
  /// requires !dat_binds.is_empty()
  Datatype(Vec<DatBind<I>>, Vec<TyBind<I>>),
  DatatypeCopy(Located<I>, Long<I>),
  /// requires !dat_binds.is_empty()
  Abstype(Vec<DatBind<I>>, Vec<TyBind<I>>, Box<Located<Dec<I>>>),
  /// requires !ex_binds.is_empty()
  Exception(Vec<ExBind<I>>),
  /// The first is the `local ... in`, the second is the `in ... end`.
  Local(Box<Located<Dec<I>>>, Box<Located<Dec<I>>>),
  /// requires !opens.is_empty()
  Open(Vec<Long<I>>),
  /// requires decs.len() >= 2
  Seq(Vec<Located<Dec<I>>>),
  // the fixity specifiers are not generic over the type of identifier because the identifiers
  // mentioned are not resolved. these declarations only affect the behavior of parsing.
  /// requires !vids.is_empty()
  Infix(Located<u32>, Vec<Located<StrRef>>),
  /// requires !vids.is_empty()
  Infixr(Located<u32>, Vec<Located<StrRef>>),
  /// requires !vids.is_empty()
  Nonfix(Vec<Located<StrRef>>),
}

/// A value binding in a `val` dec.
#[derive(Debug)]
pub struct ValBind<I> {
  /// Whether it's recursive.
  pub rec: bool,
  /// The pattern.
  pub pat: Located<Pat<I>>,
  /// The expression.
  pub exp: Located<Exp<I>>,
}

/// A function value binding in a `fun` dec.
#[derive(Debug)]
pub struct FValBind<I> {
  /// requires !cases.is_empty()
  pub cases: Vec<FValBindCase<I>>,
}

/// A case in a function value binding.
#[derive(Debug)]
pub struct FValBindCase<I> {
  /// The name of the function.
  pub vid: Located<I>,
  /// The patterns. requires !pats.is_empty()
  pub pats: Vec<Located<Pat<I>>>,
  /// The optional annotated return type.
  pub ret_ty: Option<Located<Ty<I>>>,
  /// The body.
  pub body: Located<Exp<I>>,
}

/// A type binding in a `type` dec.
#[derive(Debug)]
pub struct TyBind<I> {
  /// The type variables preceding the type constructor.
  pub ty_vars: Vec<Located<TyVar<I>>>,
  /// The type constructor, which is being defined.
  pub ty_con: Located<I>,
  /// The type which the ty con is equal to.
  pub ty: Located<Ty<I>>,
}

/// A datatype binding in a `datatype` dec. Also doubles as DatDesc.
#[derive(Debug)]
pub struct DatBind<I> {
  /// The type variables preceding the type constructor.
  pub ty_vars: Vec<Located<TyVar<I>>>,
  /// The type constructor, which is being defined.
  pub ty_con: Located<I>,
  /// The constructors of this type. requires !cons.is_empty()
  pub cons: Vec<ConBind<I>>,
}

/// A constructor binding, the rhs of a `datatype` dec. Also doubles as ConDesc.
#[derive(Debug)]
pub struct ConBind<I> {
  /// The name of the constructor.
  pub vid: Located<I>,
  /// The optional argument of this constructor.
  pub ty: Option<Located<Ty<I>>>,
}

/// An exception binding in an `exception` dec.
#[derive(Debug)]
pub struct ExBind<I> {
  /// The name of the exception.
  pub vid: Located<I>,
  /// The innards of this exception binding.
  pub inner: ExBindInner<I>,
}

/// The innards of an exception binding innards.
#[derive(Debug)]
pub enum ExBindInner<I> {
  /// None if this was just `exception E`. Some(ty) if this was `exception E of ty`.
  Ty(Option<Located<Ty<I>>>),
  /// This was `exception E = Other`, and here's the `Other`.
  Long(Long<I>),
}

/// A pattern.
#[derive(Debug)]
#[allow(missing_docs)]
pub enum Pat<I> {
  Wildcard,
  // begin special constants (NOTE no real)
  DecInt(i32),
  HexInt(i32),
  DecWord(i32),
  HexWord(i32),
  String(StrRef),
  Char(u8),
  // end special constants
  LongVid(Long<I>),
  /// the loc is for the `...` if there was one
  Record(Vec<Row<Located<Pat<I>>>>, Option<Loc>),
  /// requires pats.len() != 1
  Tuple(Vec<Located<Pat<I>>>),
  List(Vec<Located<Pat<I>>>),
  Ctor(Long<I>, Box<Located<Pat<I>>>),
  InfixCtor(Box<Located<Pat<I>>>, Located<I>, Box<Located<Pat<I>>>),
  Typed(Box<Located<Pat<I>>>, Located<Ty<I>>),
  As(Located<I>, Option<Located<Ty<I>>>, Box<Located<Pat<I>>>),
}

/// A type.
#[derive(Debug)]
#[allow(missing_docs)]
pub enum Ty<I> {
  TyVar(TyVar<I>),
  Record(Vec<Row<Located<Ty<I>>>>),
  /// requires tys.len() >= 2
  Tuple(Vec<Located<Ty<I>>>),
  TyCon(Vec<Located<Ty<I>>>, Long<I>),
  Arrow(Box<Located<Ty<I>>>, Box<Located<Ty<I>>>),
}

/// A structure expression.
#[derive(Debug)]
#[allow(missing_docs)]
pub enum StrExp<I> {
  Struct(Located<StrDec<I>>),
  LongStrId(Long<I>),
  /// The bool is false when transparent, true when opaque.
  Ascription(Box<Located<StrExp<I>>>, Located<SigExp<I>>, bool),
  FunctorApp(Located<I>, Box<Located<StrExp<I>>>),
  Let(Located<StrDec<I>>, Box<Located<StrExp<I>>>),
}

/// A structure declaration.
#[derive(Debug)]
#[allow(missing_docs)]
pub enum StrDec<I> {
  Dec(Located<Dec<I>>),
  /// requires !str_binds.is_empty()
  Structure(Vec<StrBind<I>>),
  /// The first is the `local ... in`, the second is the `in ... end`.
  Local(Box<Located<StrDec<I>>>, Box<Located<StrDec<I>>>),
  /// requires str_decs.len() != 1
  Seq(Vec<Located<StrDec<I>>>),
}

/// A structure binding.
#[derive(Debug)]
pub struct StrBind<I> {
  /// The name of the structure being bound.
  pub id: Located<I>,
  /// The expression.
  pub exp: Located<StrExp<I>>,
}

/// A signature expression.
#[derive(Debug)]
#[allow(missing_docs)]
pub enum SigExp<I> {
  Sig(Located<Spec<I>>),
  SigId(Located<I>),
  Where(
    Box<Located<SigExp<I>>>,
    Vec<Located<TyVar<I>>>,
    Long<I>,
    Located<Ty<I>>,
  ),
}

/// A signature binding.
#[derive(Debug)]
pub struct SigBind<I> {
  /// The name of the signature being bound.
  pub id: Located<I>,
  /// The expression.
  pub exp: Located<SigExp<I>>,
}

/// A specification, the guts of a signature.
#[derive(Debug)]
#[allow(missing_docs)]
pub enum Spec<I> {
  /// requires !val_descs.is_empty()
  Val(Vec<ValDesc<I>>),
  /// requires !ty_descs.is_empty(). the bool is true iff this was `eqtype`, false if it was `type`.
  Type(Vec<TyDesc<I>>, bool),
  /// requires !dat_descs.is_empty()
  Datatype(Vec<DatBind<I>>),
  DatatypeCopy(Located<I>, Long<I>),
  /// requires !ex_descs.is_empty()
  Exception(Vec<ExDesc<I>>),
  /// requires !str_descs.is_empty()
  Structure(Vec<StrDesc<I>>),
  Include(Box<Located<SigExp<I>>>),
  /// requires specs.len() != 1
  Seq(Vec<Located<Spec<I>>>),
  Sharing(Box<Located<Spec<I>>>, Vec<Long<I>>),
}

/// A value description.
#[derive(Debug)]
pub struct ValDesc<I> {
  /// The name of the value being described.
  pub vid: Located<I>,
  /// Its type.
  pub ty: Located<Ty<I>>,
}

/// A type description.
#[derive(Debug)]
pub struct TyDesc<I> {
  /// The type variables.
  pub ty_vars: Vec<Located<TyVar<I>>>,
  /// The name of the type.
  pub ty_con: Located<I>,
}

/// An exception description.
#[derive(Debug)]
pub struct ExDesc<I> {
  /// The name of the exception.
  pub vid: Located<I>,
  /// The optional type this exception constructor accepts.
  pub ty: Option<Located<Ty<I>>>,
}

/// A structure description.
#[derive(Debug)]
pub struct StrDesc<I> {
  /// The name of the structure.
  pub str_id: Located<I>,
  /// A signature expression describing it.
  pub exp: Located<SigExp<I>>,
}

/// A functor binding.
#[derive(Debug)]
pub struct FunBind<I> {
  /// The name of the functor.
  pub fun_id: Located<I>,
  /// The name of the argument structure.
  pub str_id: Located<I>,
  /// The signature the argument structure conforms to.
  pub sig_exp: Located<SigExp<I>>,
  /// The output structure.
  pub str_exp: Located<StrExp<I>>,
}

/// A top-level declaration.
#[derive(Debug)]
pub enum TopDec<I> {
  /// A structure declaration.
  StrDec(Located<StrDec<I>>),
  /// A signature declaration. requires !sig_binds.is_empty()
  SigDec(Vec<SigBind<I>>),
  /// A functor declaration. requires !fun_binds.is_empty()
  FunDec(Vec<FunBind<I>>),
}

/// Precedence of type operations.
#[derive(PartialEq, Eq, PartialOrd, Ord)]
pub enum TyPrec {
  /// `->` precedence, the lowest.
  Arrow,
  /// `*` precedence.
  Star,
  /// Application precedence (as in `int list` in which `int` is applied to `list`), the highest.
  App,
}

#[test]
fn test_ty_prec() {
  assert!(TyPrec::Arrow < TyPrec::Star);
  assert!(TyPrec::Star < TyPrec::App);
}
