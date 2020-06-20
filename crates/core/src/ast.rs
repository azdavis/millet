//! Abstract syntax trees.

use crate::intern::StrRef;
use crate::loc::{Loc, Located};
use crate::token::TyVar;

#[derive(Debug)]
pub enum Exp<I> {
  // begin special constants
  DecInt(i32),
  HexInt(i32),
  DecWord(i32),
  HexWord(i32),
  Real(f64),
  Str(StrRef),
  Char(u8),
  // end special constants
  LongVid(Long<I>),
  Record(Vec<Row<I>>),
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

#[derive(Debug)]
pub struct Long<I> {
  pub structures: Vec<Located<I>>,
  pub last: Located<I>,
}

#[derive(Debug)]
pub struct Row<I> {
  pub lab: Located<Label>,
  pub exp: Located<Exp<I>>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Label {
  Vid(StrRef),
  Num(u32),
}

/// called a 'match' in the Definition. we call it 'cases' to avoid conflicts with the Rust keyword.
#[derive(Debug)]
pub struct Cases<I> {
  /// requires !arms.is_empty()
  pub arms: Vec<Arm<I>>,
}

#[derive(Debug)]
pub struct Arm<I> {
  pub pat: Located<Pat<I>>,
  pub exp: Located<Exp<I>>,
}

#[derive(Debug)]
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

#[derive(Debug)]
pub struct ValBind<I> {
  pub rec: bool,
  pub pat: Located<Pat<I>>,
  pub exp: Located<Exp<I>>,
}

#[derive(Debug)]
pub struct FValBind<I> {
  /// requires !cases.is_empty()
  pub cases: Vec<FValBindCase<I>>,
}

#[derive(Debug)]
pub struct FValBindCase<I> {
  pub vid: Located<I>,
  /// requires !pats.is_empty()
  pub pats: Vec<Located<Pat<I>>>,
  pub ret_ty: Option<Located<Ty<I>>>,
  pub body: Located<Exp<I>>,
}

#[derive(Debug)]
pub struct TyBind<I> {
  pub ty_vars: Vec<Located<TyVar<I>>>,
  pub ty_con: Located<I>,
  pub ty: Located<Ty<I>>,
}

#[derive(Debug)]
pub struct DatBind<I> {
  pub ty_vars: Vec<Located<TyVar<I>>>,
  pub ty_con: Located<I>,
  /// requires !cons.is_empty()
  pub cons: Vec<ConBind<I>>,
}

#[derive(Debug)]
pub struct ConBind<I> {
  pub vid: Located<I>,
  pub ty: Option<Located<Ty<I>>>,
}

#[derive(Debug)]
pub struct ExBind<I> {
  pub vid: Located<I>,
  pub inner: ExBindInner<I>,
}

#[derive(Debug)]
pub enum ExBindInner<I> {
  Ty(Option<Located<Ty<I>>>),
  Long(Long<I>),
}

#[derive(Debug)]
pub enum Pat<I> {
  Wildcard,
  // begin special constants (note: no real)
  DecInt(i32),
  HexInt(i32),
  DecWord(i32),
  HexWord(i32),
  Str(StrRef),
  Char(u8),
  // end special constants
  LongVid(Long<I>),
  /// the loc is for the `...` if there was one
  Record(Vec<PatRow<I>>, Option<Loc>),
  /// requires pats.len() != 1
  Tuple(Vec<Located<Pat<I>>>),
  List(Vec<Located<Pat<I>>>),
  Ctor(Long<I>, Box<Located<Pat<I>>>),
  InfixCtor(Box<Located<Pat<I>>>, Located<I>, Box<Located<Pat<I>>>),
  Typed(Box<Located<Pat<I>>>, Located<Ty<I>>),
  As(Located<I>, Option<Located<Ty<I>>>, Box<Located<Pat<I>>>),
}

#[derive(Debug)]
pub enum PatRow<I> {
  LabelAndPat(Located<Label>, Located<Pat<I>>),
  LabelAsVid(Located<I>, Option<Located<Ty<I>>>, Option<Located<Pat<I>>>),
}

#[derive(Debug)]
pub enum Ty<I> {
  TyVar(TyVar<I>),
  Record(Vec<TyRow<I>>),
  TyCon(Vec<Located<Ty<I>>>, Long<I>),
  /// requires tys.len() >= 2
  Tuple(Vec<Located<Ty<I>>>),
  Arrow(Box<Located<Ty<I>>>, Box<Located<Ty<I>>>),
}

#[derive(Debug)]
pub struct TyRow<I> {
  pub lab: Located<Label>,
  pub ty: Located<Ty<I>>,
}

#[derive(Debug)]
pub enum StrExp<I> {
  Struct(Located<StrDec<I>>),
  LongStrId(Long<I>),
  Transparent(Box<Located<StrExp<I>>>, Located<SigExp<I>>),
  Opaque(Box<Located<StrExp<I>>>, Located<SigExp<I>>),
  FunctorApp(Long<I>, Box<Located<StrExp<I>>>),
  Let(Located<StrDec<I>>, Box<Located<StrExp<I>>>),
}

#[derive(Debug)]
pub enum StrDec<I> {
  Dec(Located<Dec<I>>),
  /// requires !str_binds.is_empty()
  Structure(Vec<StrBind<I>>),
  Local(Box<Located<StrDec<I>>>, Box<Located<StrDec<I>>>),
  /// requires specs.len() != 1
  Seq(Vec<Located<StrDec<I>>>),
}

#[derive(Debug)]
pub struct StrBind<I> {
  pub id: Located<I>,
  pub exp: Located<StrExp<I>>,
}

#[derive(Debug)]
pub enum SigExp<I> {
  Sig(Located<Spec<I>>),
  SigId(Long<I>),
  Where(
    Box<Located<SigExp<I>>>,
    Vec<Located<TyVar<I>>>,
    Long<I>,
    Located<Ty<I>>,
  ),
}

#[derive(Debug)]
pub struct SigBind<I> {
  pub id: Located<I>,
  pub exp: Located<SigExp<I>>,
}

#[derive(Debug)]
pub enum Spec<I> {
  /// requires !val_descs.is_empty()
  Val(Vec<ValDesc<I>>),
  /// requires !ty_descs.is_empty()
  Type(Vec<TyDesc<I>>),
  /// requires !ty_descs.is_empty()
  Eqtype(Vec<TyDesc<I>>),
  /// requires !dat_descs.is_empty()
  Datatype(Vec<DatDesc<I>>),
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

#[derive(Debug)]
pub struct ValDesc<I> {
  pub vid: Located<I>,
  pub ty: Located<Ty<I>>,
}

#[derive(Debug)]
pub struct TyDesc<I> {
  pub ty_vars: Vec<Located<TyVar<I>>>,
  pub ty_con: Located<I>,
}

#[derive(Debug)]
pub struct DatDesc<I> {
  pub ty_vars: Vec<Located<TyVar<I>>>,
  pub ty_con: Located<I>,
  /// requires !cons.is_empty()
  pub cons: Vec<ConDesc<I>>,
}

#[derive(Debug)]
pub struct ConDesc<I> {
  pub vid: Located<I>,
  pub ty: Option<Located<Ty<I>>>,
}

#[derive(Debug)]
pub struct ExDesc<I> {
  pub vid: Located<I>,
  pub ty: Option<Located<Ty<I>>>,
}

#[derive(Debug)]
pub struct StrDesc<I> {
  pub str_id: Located<I>,
  pub exp: Located<SigExp<I>>,
}

#[derive(Debug)]
pub struct FunBind<I> {
  pub fun_id: Located<I>,
  pub str_id: Located<I>,
  pub sig_exp: Located<SigExp<I>>,
  pub str_exp: Located<StrExp<I>>,
}

#[derive(Debug)]
pub enum TopDec<I> {
  StrDec(Located<StrDec<I>>),
  /// requires !sig_binds.is_empty()
  SigDec(Vec<SigBind<I>>),
  /// requires !fun_binds.is_empty()
  FunDec(Vec<FunBind<I>>),
}
