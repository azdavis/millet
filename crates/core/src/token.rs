#[derive(Debug, Clone)]
pub enum Token {
  // reserved words
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
  /// not a reserved word. only used in qualified names. not strictly speaking
  /// an "item of lexical analysis" as per the Definition but it's easier to
  /// handle it as such and figure out the qualified names later (in parsing).
  Dot,
  /// (maybe) numeric label (otherwise just an integer). the i64 inside will be
  /// greater than 0.
  MaybeNumLab(i32),
  // special constants (char is represented by 1-len String)
  DecInt(i32),
  HexInt(i32),
  DecWord(i32),
  HexWord(i32),
  Real(f64),
  Str(String),
  // identifiers. we can't know the syntax class of most identifiers (VId,
  // TyCon, Lab, StrId) without having the lexer be sophisticated to the point
  // of essentially being a parser. but, we can determine whether something is a
  // TyVar, and we can also know whether something might be a valid StrId.
  TyVar(TyVar),
  /// maybe a structure identifier (alphanumeric and doesn't start with prime).
  AlphaNumId(String),
  /// definitely not a structure identifier (symbolic).
  SymbolicId(String),
}

#[derive(Debug, Clone)]
pub struct TyVar {
  pub name: String,
  pub equality: bool,
}

/// These look like alphanumeric identifiers. Sorted first by length, then
/// alphabetically.
pub const ALPHA: [(&[u8], Token); 32] = [
  // 9
  (b"exception", Token::Exception),
  // 8
  (b"datatype", Token::Datatype),
  (b"withtype", Token::Withtype),
  // 7
  (b"abstype", Token::Abstype),
  (b"andalso", Token::Andalso),
  // 6
  (b"handle", Token::Handle),
  (b"infixr", Token::Infixr),
  (b"nonfix", Token::Nonfix),
  (b"orelse", Token::Orelse),
  // 5
  (b"infix", Token::Infix),
  (b"local", Token::Local),
  (b"raise", Token::Raise),
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
pub const SYMBOLIC: [(&[u8], Token); 6] = [
  // 2
  (b"->", Token::Arrow),
  (b"=>", Token::BigArrow),
  // 1
  (b":", Token::Colon),
  (b"#", Token::Pound),
  (b"=", Token::Equal),
  (b"|", Token::Bar),
];

/// These can't be mistake for identifiers. Sorted first by length, then alphabetically.
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
