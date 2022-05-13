//! Abstract syntax trees.

#![deny(missing_debug_implementations)]
#![deny(missing_docs)]
#![deny(rust_2018_idioms)]

use old_intern::StrRef;
use old_loc::{Loc, Located};
use old_token::TyVar;

/// An expression.
#[derive(Debug)]
#[allow(missing_docs)]
pub enum Exp {
  // begin special constants
  DecInt(i32),
  HexInt(i32),
  DecWord(i32),
  HexWord(i32),
  Real(f64),
  String(StrRef),
  Char(u8),
  // end special constants
  LongVid(Long),
  Record(Vec<Row<Located<Exp>>>),
  Select(Located<Label>),
  /// requires vec.len() != 1
  Tuple(Vec<Located<Exp>>),
  List(Vec<Located<Exp>>),
  /// requires vec.len() >= 2
  Sequence(Vec<Located<Exp>>),
  /// requires !vec.is_empty()
  Let(Located<Dec>, Vec<Located<Exp>>),
  App(Box<Located<Exp>>, Box<Located<Exp>>),
  InfixApp(Box<Located<Exp>>, Located<StrRef>, Box<Located<Exp>>),
  Typed(Box<Located<Exp>>, Located<Ty>),
  Andalso(Box<Located<Exp>>, Box<Located<Exp>>),
  Orelse(Box<Located<Exp>>, Box<Located<Exp>>),
  Handle(Box<Located<Exp>>, Cases),
  Raise(Box<Located<Exp>>),
  If(Box<Located<Exp>>, Box<Located<Exp>>, Box<Located<Exp>>),
  While(Box<Located<Exp>>, Box<Located<Exp>>),
  Case(Box<Located<Exp>>, Cases),
  Fn(Cases),
}

/// A long identifier. Can have zero or more structures qualifying the identifier.
#[derive(Debug)]
pub struct Long {
  /// The structures qualifying this identifier, in order.
  pub structures: Vec<Located<StrRef>>,
  /// The final component of the identifier, after all of the zero or more structures
  pub last: Located<StrRef>,
}

impl Long {
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
pub struct Cases {
  /// requires !arms.is_empty()
  pub arms: Vec<Arm>,
}

/// An arm of a match expression (we call it "Cases").
#[derive(Debug)]
pub struct Arm {
  /// The pattern.
  pub pat: Located<Pat>,
  /// The expression.
  pub exp: Located<Exp>,
}

/// A declaration.
#[derive(Debug)]
#[allow(missing_docs)]
pub enum Dec {
  /// requires !val_binds.is_empty()
  Val(Vec<Located<TyVar>>, Vec<ValBind>),
  /// requires !fval_binds.is_empty()
  Fun(Vec<Located<TyVar>>, Vec<FValBind>),
  /// requires !ty_binds.is_empty()
  Type(Vec<TyBind>),
  /// requires !dat_binds.is_empty()
  Datatype(Vec<DatBind>, Vec<TyBind>),
  DatatypeCopy(Located<StrRef>, Long),
  /// requires !dat_binds.is_empty()
  Abstype(Vec<DatBind>, Vec<TyBind>, Box<Located<Dec>>),
  /// requires !ex_binds.is_empty()
  Exception(Vec<ExBind>),
  /// The first is the `local ... in`, the second is the `in ... end`.
  Local(Box<Located<Dec>>, Box<Located<Dec>>),
  /// requires !opens.is_empty()
  Open(Vec<Long>),
  /// requires decs.len() >= 2
  Seq(Vec<Located<Dec>>),
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
pub struct ValBind {
  /// Whether it's recursive.
  pub rec: bool,
  /// The pattern.
  pub pat: Located<Pat>,
  /// The expression.
  pub exp: Located<Exp>,
}

/// A function value binding in a `fun` dec.
#[derive(Debug)]
pub struct FValBind {
  /// requires !cases.is_empty()
  pub cases: Vec<FValBindCase>,
}

/// A case in a function value binding.
#[derive(Debug)]
pub struct FValBindCase {
  /// The name of the function.
  pub vid: Located<StrRef>,
  /// The patterns. requires !pats.is_empty()
  pub pats: Vec<Located<Pat>>,
  /// The optional annotated return type.
  pub ret_ty: Option<Located<Ty>>,
  /// The body.
  pub body: Located<Exp>,
}

/// A type binding in a `type` dec.
#[derive(Debug)]
pub struct TyBind {
  /// The type variables preceding the type constructor.
  pub ty_vars: Vec<Located<TyVar>>,
  /// The type constructor, which is being defined.
  pub ty_con: Located<StrRef>,
  /// The type which the ty con is equal to.
  pub ty: Located<Ty>,
}

/// A datatype binding in a `datatype` dec. Also doubles as DatDesc.
#[derive(Debug)]
pub struct DatBind {
  /// The type variables preceding the type constructor.
  pub ty_vars: Vec<Located<TyVar>>,
  /// The type constructor, which is being defined.
  pub ty_con: Located<StrRef>,
  /// The constructors of this type. requires !cons.is_empty()
  pub cons: Vec<ConBind>,
}

/// A constructor binding, the rhs of a `datatype` dec. Also doubles as ConDesc.
#[derive(Debug)]
pub struct ConBind {
  /// The name of the constructor.
  pub vid: Located<StrRef>,
  /// The optional argument of this constructor.
  pub ty: Option<Located<Ty>>,
}

/// An exception binding in an `exception` dec.
#[derive(Debug)]
pub struct ExBind {
  /// The name of the exception.
  pub vid: Located<StrRef>,
  /// The innards of this exception binding.
  pub inner: ExBindInner,
}

/// The innards of an exception binding innards.
#[derive(Debug)]
pub enum ExBindInner {
  /// None if this was just `exception E`. Some(ty) if this was `exception E of ty`.
  Ty(Option<Located<Ty>>),
  /// This was `exception E = Other`, and here's the `Other`.
  Long(Long),
}

/// A pattern.
#[derive(Debug)]
#[allow(missing_docs)]
pub enum Pat {
  Wildcard,
  // begin special constants (NOTE no real)
  DecInt(i32),
  HexInt(i32),
  DecWord(i32),
  HexWord(i32),
  String(StrRef),
  Char(u8),
  // end special constants
  LongVid(Long),
  /// the loc is for the `...` if there was one
  Record(Vec<Row<Located<Pat>>>, Option<Loc>),
  /// requires pats.len() != 1
  Tuple(Vec<Located<Pat>>),
  List(Vec<Located<Pat>>),
  Ctor(Long, Box<Located<Pat>>),
  InfixCtor(Box<Located<Pat>>, Located<StrRef>, Box<Located<Pat>>),
  Typed(Box<Located<Pat>>, Located<Ty>),
  As(Located<StrRef>, Option<Located<Ty>>, Box<Located<Pat>>),
}

/// A type.
#[derive(Debug)]
#[allow(missing_docs)]
pub enum Ty {
  TyVar(TyVar),
  Record(Vec<Row<Located<Ty>>>),
  /// requires tys.len() >= 2
  Tuple(Vec<Located<Ty>>),
  TyCon(Vec<Located<Ty>>, Long),
  Arrow(Box<Located<Ty>>, Box<Located<Ty>>),
}

/// A structure expression.
#[derive(Debug)]
#[allow(missing_docs)]
pub enum StrExp {
  Struct(Located<StrDec>),
  LongStrId(Long),
  /// The bool is false when transparent, true when opaque.
  Ascription(Box<Located<StrExp>>, Located<SigExp>, bool),
  FunctorApp(Located<StrRef>, Box<Located<StrExp>>),
  Let(Located<StrDec>, Box<Located<StrExp>>),
}

/// A structure declaration.
#[derive(Debug)]
#[allow(missing_docs)]
pub enum StrDec {
  Dec(Located<Dec>),
  /// requires !str_binds.is_empty()
  Structure(Vec<StrBind>),
  /// The first is the `local ... in`, the second is the `in ... end`.
  Local(Box<Located<StrDec>>, Box<Located<StrDec>>),
  /// requires str_decs.len() != 1
  Seq(Vec<Located<StrDec>>),
}

/// A structure binding.
#[derive(Debug)]
pub struct StrBind {
  /// The name of the structure being bound.
  pub id: Located<StrRef>,
  /// The expression.
  pub exp: Located<StrExp>,
}

/// A signature expression.
#[derive(Debug)]
#[allow(missing_docs)]
pub enum SigExp {
  Sig(Located<Spec>),
  SigId(Located<StrRef>),
  Where(Box<Located<SigExp>>, Vec<Located<TyVar>>, Long, Located<Ty>),
}

/// A signature binding.
#[derive(Debug)]
pub struct SigBind {
  /// The name of the signature being bound.
  pub id: Located<StrRef>,
  /// The expression.
  pub exp: Located<SigExp>,
}

/// A specification, the guts of a signature.
#[derive(Debug)]
#[allow(missing_docs)]
pub enum Spec {
  /// requires !val_descs.is_empty()
  Val(Vec<ValDesc>),
  /// requires !ty_descs.is_empty(). the bool is true iff this was `eqtype`, false if it was `type`.
  Type(Vec<TyDesc>, bool),
  /// requires !dat_descs.is_empty()
  Datatype(Vec<DatBind>),
  DatatypeCopy(Located<StrRef>, Long),
  /// requires !ex_descs.is_empty()
  Exception(Vec<ExDesc>),
  /// requires !str_descs.is_empty()
  Structure(Vec<StrDesc>),
  Include(Box<Located<SigExp>>),
  /// requires specs.len() != 1
  Seq(Vec<Located<Spec>>),
  Sharing(Box<Located<Spec>>, Vec<Long>),
}

/// A value description.
#[derive(Debug)]
pub struct ValDesc {
  /// The name of the value being described.
  pub vid: Located<StrRef>,
  /// Its type.
  pub ty: Located<Ty>,
}

/// A type description.
#[derive(Debug)]
pub struct TyDesc {
  /// The type variables.
  pub ty_vars: Vec<Located<TyVar>>,
  /// The name of the type.
  pub ty_con: Located<StrRef>,
}

/// An exception description.
#[derive(Debug)]
pub struct ExDesc {
  /// The name of the exception.
  pub vid: Located<StrRef>,
  /// The optional type this exception constructor accepts.
  pub ty: Option<Located<Ty>>,
}

/// A structure description.
#[derive(Debug)]
pub struct StrDesc {
  /// The name of the structure.
  pub str_id: Located<StrRef>,
  /// A signature expression describing it.
  pub exp: Located<SigExp>,
}

/// A functor binding.
#[derive(Debug)]
pub struct FunBind {
  /// The name of the functor.
  pub fun_id: Located<StrRef>,
  /// The name of the argument structure.
  pub str_id: Located<StrRef>,
  /// The signature the argument structure conforms to.
  pub sig_exp: Located<SigExp>,
  /// The output structure.
  pub str_exp: Located<StrExp>,
}

/// A top-level declaration.
#[derive(Debug)]
pub enum TopDec {
  /// A structure declaration.
  StrDec(Located<StrDec>),
  /// A signature declaration. requires !sig_binds.is_empty()
  SigDec(Vec<SigBind>),
  /// A functor declaration. requires !fun_binds.is_empty()
  FunDec(Vec<FunBind>),
}

/// Precedence of type operations.
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum TyPrec {
  /// `->` precedence, the lowest.
  Arrow,
  /// `*` precedence.
  Star,
  /// Application precedence (as in `int list` in which `int` is applied to `list`), the highest.
  App,
}
