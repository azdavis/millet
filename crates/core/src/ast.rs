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
  Select(Label),
  /// requires vec.len() != 1
  Tuple(Vec<Exp<I>>),
  List(Vec<Exp<I>>),
  /// requires vec.len() >= 2
  Sequence(Vec<Exp<I>>),
  /// requires vec.len != 0
  Let(Box<Dec<I>>, Vec<Exp<I>>),
  App(Box<Exp<I>>, Box<Exp<I>>),
  InfixApp(Box<Exp<I>>, I, Box<Exp<I>>),
  Typed(Box<Exp<I>>, Ty<I>),
  Andalso(Box<Exp<I>>, Box<Exp<I>>),
  Orelse(Box<Exp<I>>, Box<Exp<I>>),
  Handle(Box<Exp<I>>, Match<I>),
  Raise(Box<Exp<I>>),
  If(Box<Exp<I>>, Box<Exp<I>>, Box<Exp<I>>),
  While(Box<Exp<I>>, Box<Exp<I>>),
  Case(Box<Exp<I>>, Match<I>),
  Fn(Match<I>),
}

pub struct Long<I> {
  /// requires !idents.is_empty()
  idents: Vec<I>,
}

pub struct Row<I> {
  lab: Label,
  exp: Exp<I>,
}

pub enum Label {
  Vid(String),
  Num(u32),
}

pub struct Match<I> {
  arms: Vec<Arm<I>>,
}

pub struct Arm<I> {
  pat: Pat<I>,
  exp: Exp<I>,
}

pub enum Dec<I> {
  /// requires !val_binds.is_empty()
  Val(Vec<TyVar<I>>, Vec<ValBind<I>>),
  /// requires !fval_binds.is_empty()
  Fun(Vec<TyVar<I>>, Vec<FValBind<I>>),
  /// requires !typ_binds.is_empty()
  Type(Vec<TypBind<I>>),
  /// requires !dat_binds.is_empty()
  Datatype(Vec<DatBind<I>>, Vec<TypBind<I>>),
  DatatypeCopy(I, Long<I>),
  /// requires !dat_binds.is_empty()
  Abstype(Vec<DatBind<I>>, Vec<TypBind<I>>, Box<Dec<I>>),
  /// requires !ex_binds.is_empty()
  Exception(Vec<ExBind<I>>),
  Local(Box<Dec<I>>, Box<Dec<I>>),
  /// requires !opens.is_empty()
  Open(Vec<Long<I>>),
  /// requires decs.len() >= 2
  Seq(Vec<Dec<I>>),
  /// requires d >= 1 && !vids.is_empty()
  Infix(u32, Vec<I>),
  /// requires d >= 1 && !vids.is_empty()
  Infixr(u32, Vec<I>),
  /// requires !vids.is_empty()
  Nonfix(Vec<I>),
}

pub struct ValBind<I> {
  rec: bool,
  pat: Pat<I>,
  exp: Exp<I>,
}

pub struct FValBind<I> {
  /// requires !cases.is_empty()
  cases: Vec<FValBindCase<I>>,
}

pub struct FValBindCase<I> {
  name: I,
  /// requires !pats.is_empty()
  pats: Vec<Pat<I>>,
  ret_ty: Option<Ty<I>>,
  body: Exp<I>,
}

pub struct TypBind<I> {
  ty_vars: Vec<TyVar<I>>,
  ty_con: I,
  ty: Ty<I>,
}

pub struct DatBind<I> {
  ty_vars: Vec<TyVar<I>>,
  ty_con: I,
  /// requires !cons.is_empty()
  cons: Vec<ConBind<I>>,
}

pub struct ConBind<I> {
  name: I,
  ty: Option<Ty<I>>,
}

pub struct ExBind<I> {
  name: I,
  inner: Option<ExBindInner<I>>,
}

pub enum ExBindInner<I> {
  Ty(Ty<I>),
  Long(Long<I>),
}

pub enum Pat<I> {
  Wildcard,
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
  Record(Vec<PatRow<I>>),
  /// requires pats.len() != 1
  Tuple(Vec<Pat<I>>),
  List(Vec<Pat<I>>),
  Ctor(Long<I>, Box<Pat<I>>),
  InfixCtor(Box<Pat<I>>, I, Box<Pat<I>>),
  Typed(Box<Pat<I>>, Ty<I>),
  As(I, Option<Ty<I>>, Box<Pat<I>>),
}

pub enum PatRow<I> {
  DotDotDot,
  LabelAndPat(Label, Pat<I>),
  LabelAsVid(I, Option<Ty<I>>, Option<Pat<I>>),
}

pub enum Ty<I> {
  TyVar(TyVar<I>),
  Record(Vec<TyRow<I>>),
  TyCon(Vec<Ty<I>>, Long<I>),
  /// requires tys.len() >= 2
  Tuple(Vec<Ty<I>>),
  Arrow(Box<Ty<I>>, Box<Ty<I>>),
}

pub struct TyRow<I> {
  lab: Label,
  ty: Ty<I>,
}
