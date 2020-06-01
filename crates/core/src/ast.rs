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
  pub idents: Vec<I>,
}

pub struct Row<I> {
  pub lab: Label,
  pub exp: Exp<I>,
}

pub enum Label {
  Vid(String),
  Num(u32),
}

pub struct Match<I> {
  pub arms: Vec<Arm<I>>,
}

pub struct Arm<I> {
  pub pat: Pat<I>,
  pub exp: Exp<I>,
}

pub enum Dec<I> {
  /// requires !val_binds.is_empty()
  Val(Vec<TyVar<I>>, Vec<ValBind<I>>),
  /// requires !fval_binds.is_empty()
  Fun(Vec<TyVar<I>>, Vec<FValBind<I>>),
  /// requires !ty_binds.is_empty()
  Type(Vec<TyBind<I>>),
  /// requires !dat_binds.is_empty()
  Datatype(Vec<DatBind<I>>, Vec<TyBind<I>>),
  DatatypeCopy(I, Long<I>),
  /// requires !dat_binds.is_empty()
  Abstype(Vec<DatBind<I>>, Vec<TyBind<I>>, Box<Dec<I>>),
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
  pub rec: bool,
  pub pat: Pat<I>,
  pub exp: Exp<I>,
}

pub struct FValBind<I> {
  /// requires !cases.is_empty()
  pub cases: Vec<FValBindCase<I>>,
}

pub struct FValBindCase<I> {
  pub vid: I,
  /// requires !pats.is_empty()
  pub pats: Vec<Pat<I>>,
  pub ret_ty: Option<Ty<I>>,
  pub body: Exp<I>,
}

pub struct TyBind<I> {
  pub ty_vars: Vec<TyVar<I>>,
  pub ty_con: I,
  pub ty: Ty<I>,
}

pub struct DatBind<I> {
  pub ty_vars: Vec<TyVar<I>>,
  pub ty_con: I,
  /// requires !cons.is_empty()
  pub cons: Vec<ConBind<I>>,
}

pub struct ConBind<I> {
  pub vid: I,
  pub ty: Option<Ty<I>>,
}

pub struct ExBind<I> {
  pub vid: I,
  pub inner: Option<ExBindInner<I>>,
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
  pub lab: Label,
  pub ty: Ty<I>,
}

pub enum StrExp<I> {
  Struct(StrDec<I>),
  LongStrId(Long<I>),
  Transparent(Box<StrExp<I>>, SigExp<I>),
  Opaque(Box<StrExp<I>>, SigExp<I>),
  FunctorApp(Long<I>, Box<StrExp<I>>),
  Let(StrDec<I>, Box<StrExp<I>>),
}

pub enum StrDec<I> {
  Dec(Dec<I>),
  /// requires !str_binds.is_empty()
  Structure(Vec<StrBind<I>>),
  Local(Box<StrDec<I>>, Box<StrDec<I>>),
}

pub struct StrBind<I> {
  pub strid: I,
  pub exp: StrExp<I>,
}

pub enum SigExp<I> {
  Sig(Spec<I>),
  SigId(Long<I>),
  Where(Box<SigExp<I>>, Vec<TyVar<I>>, Long<I>, Ty<I>),
}

pub struct SigBind<I> {
  pub sigid: I,
  pub exp: SigExp<I>,
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
  DatatypeCopy(I, Long<I>),
  /// requires !ex_descs.is_empty()
  Exception(Vec<ExDesc<I>>),
  /// requires !str_descs.is_empty()
  Structure(Vec<StrDesc<I>>),
  Include(Box<SigExp<I>>),
  /// requires specs.len() != 1
  Seq(Vec<Spec<I>>),
}

pub struct ValDesc<I> {
  pub vid: I,
  pub ty: Ty<I>,
}

pub struct TyDesc<I> {
  pub ty_vars: Vec<TyVar<I>>,
  pub ty_con: I,
}

pub struct DatDesc<I> {
  pub ty_vars: Vec<TyVar<I>>,
  pub ty_con: I,
  /// requires !con_descs.is_empty()
  pub con_descs: Vec<ConDesc<I>>,
}

pub struct ConDesc<I> {
  pub vid: I,
  pub ty: Option<Ty<I>>,
}

pub struct ExDesc<I> {
  pub vid: I,
  pub ty: Option<Ty<I>>,
}

pub struct StrDesc<I> {
  pub str_id: I,
  pub exp: SigExp<I>,
}

pub struct FunBind<I> {
  pub fun_id: Long<I>,
  pub str_id: Long<I>,
  pub sig_exp: SigExp<I>,
  pub str_exp: StrExp<I>,
}

pub enum TopDec<I> {
  StrDec(StrDec<I>),
  /// requires !sig_binds.is_empty()
  SigDec(Vec<SigBind<I>>),
  /// requires !fun_binds.is_empty()
  FunDec(Vec<FunBind<I>>),
}
