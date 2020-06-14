//! Lexical tokens.

use crate::intern::StrRef;

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum Token {
  // core reserved words
  Abstype,
  And,
  Andalso,
  As,
  Case,
  Datatype,
  Do,
  Else,
  End,
  Exception,
  Fn,
  Fun,
  Handle,
  If,
  In,
  Infix,
  Infixr,
  Let,
  Local,
  Nonfix,
  Of,
  Op,
  Open,
  Orelse,
  Raise,
  Rec,
  Then,
  Type,
  Val,
  With,
  Withtype,
  While,
  LRound,
  RRound,
  LSquare,
  RSquare,
  LCurly,
  RCurly,
  Comma,
  Colon,
  Semicolon,
  DotDotDot,
  Underscore,
  Bar,
  Equal,
  BigArrow,
  Arrow,
  Pound,
  // modules reserved words
  Eqtype,
  Functor,
  Include,
  Sharing,
  Sig,
  Signature,
  Struct,
  Structure,
  Where,
  ColonGt,
  /// not a reserved word. only used in qualified names. not strictly speaking an "item of lexical
  /// analysis" as per the Definition but it's easier to handle it as such and figure out the
  /// qualified names later (in parsing).
  Dot,
  // special constants
  DecInt(i32, IsNumLab),
  HexInt(i32),
  DecWord(i32),
  HexWord(i32),
  Real(f64),
  Str(StrRef),
  Char(u8),
  // identifiers. we can't know the syntax class of most identifiers (VId, TyCon, Lab, StrId)
  // without having the lexer be sophisticated to the point of essentially being a parser. but, we
  // can determine whether something is a TyVar, and we can also know whether something might be a
  // valid StrId.
  TyVar(TyVar<StrRef>),
  Ident(StrRef, IdentType),
  /// not actually a token, but makes the api simpler.
  EOF,
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum IsNumLab {
  Maybe,
  No,
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum IdentType {
  AlphaNum,
  Symbolic,
}

impl Token {
  pub fn desc(&self) -> &'static str {
    match self {
      Self::Abstype => "`abstype`",
      Self::And => "`and`",
      Self::Andalso => "`andalso`",
      Self::As => "`as`",
      Self::Case => "`case`",
      Self::Datatype => "`datatype`",
      Self::Do => "`do`",
      Self::Else => "`else`",
      Self::End => "`end`",
      Self::Exception => "`exception`",
      Self::Fn => "`fn`",
      Self::Fun => "`fun`",
      Self::Handle => "`handle`",
      Self::If => "`if`",
      Self::In => "`in`",
      Self::Infix => "`infix`",
      Self::Infixr => "`infixr`",
      Self::Let => "`let`",
      Self::Local => "`local`",
      Self::Nonfix => "`nonfix`",
      Self::Of => "`of`",
      Self::Op => "`op`",
      Self::Open => "`open`",
      Self::Orelse => "`orelse`",
      Self::Raise => "`raise`",
      Self::Rec => "`rec`",
      Self::Then => "`then`",
      Self::Type => "`type`",
      Self::Val => "`val`",
      Self::With => "`with`",
      Self::Withtype => "`withtype`",
      Self::While => "`while`",
      Self::LRound => "`(`",
      Self::RRound => "`)`",
      Self::LSquare => "`[`",
      Self::RSquare => "`]`",
      Self::LCurly => "`{`",
      Self::RCurly => "`}`",
      Self::Comma => "`,`",
      Self::Colon => "`:`",
      Self::Semicolon => "`;`",
      Self::DotDotDot => "`...`",
      Self::Underscore => "`_`",
      Self::Bar => "`|`",
      Self::Equal => "`=`",
      Self::BigArrow => "`=>`",
      Self::Arrow => "`->`",
      Self::Pound => "`#`",
      Self::Eqtype => "`eqtype`",
      Self::Functor => "`functor`",
      Self::Include => "`include`",
      Self::Sharing => "`sharing`",
      Self::Sig => "`sig`",
      Self::Signature => "`signature`",
      Self::Struct => "`struct`",
      Self::Structure => "`structure`",
      Self::Where => "`where`",
      Self::ColonGt => "`:>`",
      Self::Dot => "`.`",
      Self::DecInt(..) => "an integer constant",
      Self::HexInt(..) => "a hexadecimal integer constant",
      Self::DecWord(..) => "a word constant",
      Self::HexWord(..) => "a hexadecimal word constant",
      Self::Real(..) => "a real constant",
      Self::Str(..) => "a string constant",
      Self::Char(..) => "a character constant",
      Self::TyVar(..) => "a type variable",
      Self::Ident(_, IdentType::AlphaNum) => "an identifier",
      Self::Ident(_, IdentType::Symbolic) => "a symbolic identifier",
      Self::EOF => "end of file",
    }
  }
}

/// This is here (and not in ast.rs) because we know when lexing whether something is a type var.
#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub struct TyVar<I> {
  pub name: I,
  pub equality: bool,
}

/// These look like alphanumeric identifiers. Sorted first by length, then alphabetically.
pub const ALPHA: [(&[u8], Token); 41] = [
  // 9
  (b"exception", Token::Exception),
  (b"signature", Token::Signature),
  (b"structure", Token::Structure),
  // 8
  (b"datatype", Token::Datatype),
  (b"withtype", Token::Withtype),
  // 7
  (b"abstype", Token::Abstype),
  (b"andalso", Token::Andalso),
  (b"functor", Token::Functor),
  (b"include", Token::Include),
  (b"sharing", Token::Sharing),
  // 6
  (b"eqtype", Token::Eqtype),
  (b"handle", Token::Handle),
  (b"infixr", Token::Infixr),
  (b"nonfix", Token::Nonfix),
  (b"orelse", Token::Orelse),
  (b"struct", Token::Struct),
  // 5
  (b"infix", Token::Infix),
  (b"local", Token::Local),
  (b"raise", Token::Raise),
  (b"where", Token::Where),
  (b"while", Token::While),
  // 4
  (b"case", Token::Case),
  (b"else", Token::Else),
  (b"open", Token::Open),
  (b"then", Token::Then),
  (b"type", Token::Type),
  (b"with", Token::With),
  // 3
  (b"and", Token::And),
  (b"end", Token::End),
  (b"fun", Token::Fun),
  (b"let", Token::Let),
  (b"rec", Token::Rec),
  (b"sig", Token::Sig),
  (b"val", Token::Val),
  // 2
  (b"as", Token::As),
  (b"do", Token::Do),
  (b"fn", Token::Fn),
  (b"if", Token::If),
  (b"in", Token::In),
  (b"of", Token::Of),
  (b"op", Token::Op),
];

/// These look like symbolic identifiers. Sorted first by length, then
/// alphabetically.
pub const SYMBOLIC: [(&[u8], Token); 7] = [
  // 2
  (b"->", Token::Arrow),
  (b":>", Token::ColonGt),
  (b"=>", Token::BigArrow),
  // 1
  (b":", Token::Colon),
  (b"#", Token::Pound),
  (b"=", Token::Equal),
  (b"|", Token::Bar),
];

/// These can't be mistaken for identifiers. Sorted first by length, then
/// alphabetically.
pub const OTHER: [(&[u8], Token); 11] = [
  // 3
  (b"...", Token::DotDotDot),
  // 1
  (b"_", Token::Underscore),
  (b",", Token::Comma),
  (b";", Token::Semicolon),
  (b".", Token::Dot),
  (b"(", Token::LRound),
  (b")", Token::RRound),
  (b"[", Token::LSquare),
  (b"]", Token::RSquare),
  (b"{", Token::LCurly),
  (b"}", Token::RCurly),
];

#[test]
fn test_size() {
  assert_eq!(std::mem::size_of::<Token>(), 24);
}
