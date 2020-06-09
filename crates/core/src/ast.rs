//! Abstract syntax trees.

use crate::ident::Ident;
use crate::source::{Loc, Located};
use crate::token::TyVar;

pub enum Exp<I> {
  // begin special constants
  DecInt(i32),
  HexInt(i32),
  DecWord(i32),
  HexWord(i32),
  Real(f64),
  Str(String),
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
  /// requires vec.len != 0
  Let(Located<Dec<I>>, Vec<Located<Exp<I>>>),
  App(Box<Located<Exp<I>>>, Box<Located<Exp<I>>>),
  InfixApp(Box<Located<Exp<I>>>, Located<I>, Box<Located<Exp<I>>>),
  Typed(Box<Located<Exp<I>>>, Located<Ty<I>>),
  Andalso(Box<Located<Exp<I>>>, Box<Located<Exp<I>>>),
  Orelse(Box<Located<Exp<I>>>, Box<Located<Exp<I>>>),
  Handle(Box<Located<Exp<I>>>, Match<I>),
  Raise(Box<Located<Exp<I>>>),
  If(
    Box<Located<Exp<I>>>,
    Box<Located<Exp<I>>>,
    Box<Located<Exp<I>>>,
  ),
  While(Box<Located<Exp<I>>>, Box<Located<Exp<I>>>),
  Case(Box<Located<Exp<I>>>, Match<I>),
  Fn(Match<I>),
}

pub struct Long<I> {
  /// requires !idents.is_empty()
  pub idents: Vec<Located<I>>,
}

pub struct Row<I> {
  pub lab: Located<Label>,
  pub exp: Located<Exp<I>>,
}

pub enum Label {
  Vid(Ident),
  Num(u32),
}

pub struct Match<I> {
  pub arms: Vec<Arm<I>>,
}

pub struct Arm<I> {
  pub pat: Located<Pat<I>>,
  pub exp: Located<Exp<I>>,
}

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
  /// requires !vids.is_empty()
  Infix(Located<u32>, Vec<Located<I>>),
  /// requires !vids.is_empty()
  Infixr(Located<u32>, Vec<Located<I>>),
  /// requires !vids.is_empty()
  Nonfix(Vec<Located<I>>),
}

pub struct ValBind<I> {
  pub rec: bool,
  pub pat: Located<Pat<I>>,
  pub exp: Located<Exp<I>>,
}

pub struct FValBind<I> {
  /// requires !cases.is_empty()
  pub cases: Vec<FValBindCase<I>>,
}

pub struct FValBindCase<I> {
  pub vid: Located<I>,
  /// requires !pats.is_empty()
  pub pats: Vec<Located<Pat<I>>>,
  pub ret_ty: Option<Located<Ty<I>>>,
  pub body: Located<Exp<I>>,
}

pub struct TyBind<I> {
  pub ty_vars: Vec<Located<TyVar<I>>>,
  pub ty_con: Located<I>,
  pub ty: Located<Ty<I>>,
}

pub struct DatBind<I> {
  pub ty_vars: Vec<Located<TyVar<I>>>,
  pub ty_con: Located<I>,
  /// requires !cons.is_empty()
  pub cons: Vec<ConBind<I>>,
}

pub struct ConBind<I> {
  pub vid: Located<I>,
  pub ty: Option<Located<Ty<I>>>,
}

pub struct ExBind<I> {
  pub vid: Located<I>,
  pub inner: ExBindInner<I>,
}

pub enum ExBindInner<I> {
  Ty(Option<Located<Ty<I>>>),
  Long(Long<I>),
}

pub enum Pat<I> {
  Wildcard,
  // begin special constants (note: no real)
  DecInt(i32),
  HexInt(i32),
  DecWord(i32),
  HexWord(i32),
  Str(String),
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

pub enum PatRow<I> {
  LabelAndPat(Located<Label>, Located<Pat<I>>),
  LabelAsVid(Located<I>, Option<Located<Ty<I>>>, Option<Located<Pat<I>>>),
}

pub enum Ty<I> {
  TyVar(TyVar<I>),
  Record(Vec<TyRow<I>>),
  TyCon(Vec<Located<Ty<I>>>, Long<I>),
  /// requires tys.len() >= 2
  Tuple(Vec<Located<Ty<I>>>),
  Arrow(Box<Located<Ty<I>>>, Box<Located<Ty<I>>>),
}

pub struct TyRow<I> {
  pub lab: Located<Label>,
  pub ty: Located<Ty<I>>,
}

pub enum StrExp<I> {
  Struct(Located<StrDec<I>>),
  LongStrId(Long<I>),
  Transparent(Box<Located<StrExp<I>>>, Located<SigExp<I>>),
  Opaque(Box<Located<StrExp<I>>>, Located<SigExp<I>>),
  FunctorApp(Long<I>, Box<Located<StrExp<I>>>),
  Let(Located<StrDec<I>>, Box<Located<StrExp<I>>>),
}

pub enum StrDec<I> {
  Dec(Located<Dec<I>>),
  /// requires !str_binds.is_empty()
  Structure(Vec<StrBind<I>>),
  Local(Box<Located<StrDec<I>>>, Box<Located<StrDec<I>>>),
}

pub struct StrBind<I> {
  pub strid: Located<I>,
  pub exp: Located<StrExp<I>>,
}

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

pub struct SigBind<I> {
  pub sigid: Located<I>,
  pub exp: Located<SigExp<I>>,
}

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
}

pub struct ValDesc<I> {
  pub vid: Located<I>,
  pub ty: Located<Ty<I>>,
}

pub struct TyDesc<I> {
  pub ty_vars: Vec<Located<TyVar<I>>>,
  pub ty_con: Located<I>,
}

pub struct DatDesc<I> {
  pub ty_vars: Vec<Located<TyVar<I>>>,
  pub ty_con: Located<I>,
  /// requires !con_descs.is_empty()
  pub con_descs: Vec<ConDesc<I>>,
}

pub struct ConDesc<I> {
  pub vid: Located<I>,
  pub ty: Option<Located<Ty<I>>>,
}

pub struct ExDesc<I> {
  pub vid: Located<I>,
  pub ty: Option<Located<Ty<I>>>,
}

pub struct StrDesc<I> {
  pub str_id: Located<I>,
  pub exp: Located<SigExp<I>>,
}

pub struct FunBind<I> {
  pub fun_id: Long<I>,
  pub str_id: Long<I>,
  pub sig_exp: Located<SigExp<I>>,
  pub str_exp: Located<StrExp<I>>,
}

pub enum TopDec<I> {
  StrDec(Located<StrDec<I>>),
  /// requires !sig_binds.is_empty()
  SigDec(Vec<SigBind<I>>),
  /// requires !fun_binds.is_empty()
  FunDec(Vec<FunBind<I>>),
}
