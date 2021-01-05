use lexer::TokenKind;
use num_derive::{FromPrimitive, ToPrimitive};
use num_traits::{FromPrimitive, ToPrimitive};

#[derive(Debug, Copy, Clone, Ord, PartialOrd, Eq, PartialEq, Hash)]
pub enum SML {}

impl rowan::Language for SML {
  type Kind = SyntaxKind;

  fn kind_from_raw(raw: rowan::SyntaxKind) -> Self::Kind {
    Self::Kind::from_u16(raw.0).unwrap()
  }

  fn kind_to_raw(kind: Self::Kind) -> rowan::SyntaxKind {
    rowan::SyntaxKind(kind.to_u16().unwrap())
  }
}

pub type SyntaxNode = rowan::SyntaxNode<SML>;

#[derive(Debug, Copy, Clone, PartialEq, FromPrimitive, ToPrimitive)]
pub enum SyntaxKind {
  // Non-terminals
  // (TODO)
  ROOT,
  SCON,

  TOPDEC_SIG,
  TOPDEC_FUN,
  TOPDEC_STR,

  SIGEXP_ID,
  SIGEXP_SIG,
  SIGEXP_WHERE,

  EXP_CASE,
  EXP_FN,
  EXP_IF,
  EXP_RAISE,
  EXP_WHILE,

  // Terminals
  // core reserved words
  ABSTYPE,
  AND,
  ANDALSO,
  AS,
  CASE,
  DATATYPE,
  DO,
  ELSE,
  END,
  EXCEPTION,
  FN,
  FUN,
  HANDLE,
  IF,
  IN,
  INFIX,
  INFIXR,
  LET,
  LOCAL,
  NONFIX,
  OF,
  OP,
  OPEN,
  ORELSE,
  RAISE,
  REC,
  THEN,
  TYPE,
  VAL,
  WITH,
  WITHTYPE,
  WHILE,

  // module reserved words
  EQTYPE,
  FUNCTOR,
  INCLUDE,
  SHARING,
  SIG,
  SIGNATURE,
  STRUCT,
  STRUCTURE,
  WHERE,

  // misc symbols
  LPAREN,
  RPAREN,
  LSQUARE,
  RSQUARE,
  LCURLY,
  RCURLY,
  COMMA,
  COLON,
  SEMICOLON,
  DOTDOTDOT,
  UNDERSCORE,
  BAR,
  EQUAL,
  DARROW,
  ARROW,
  POUND,
  COLONGT,
  DOT,

  // special constants
  DECINT,
  HEXINT,
  DECWORD,
  HEXWORD,
  REAL,
  STRING,
  CHAR,

  // identifiers
  TYVARID,
  IDENT,

  // non-token tokens
  COMMENT,
  WHITESPACE,
  EOF,
  ERROR,
}

impl From<TokenKind> for SyntaxKind {
  fn from(token_kind: TokenKind) -> Self {
    match token_kind {
      // core reserved words
      TokenKind::ABSTYPE => Self::ABSTYPE,
      TokenKind::AND => Self::AND,
      TokenKind::ANDALSO => Self::ANDALSO,
      TokenKind::AS => Self::AS,
      TokenKind::CASE => Self::CASE,
      TokenKind::DATATYPE => Self::DATATYPE,
      TokenKind::DO => Self::DO,
      TokenKind::ELSE => Self::ELSE,
      TokenKind::END => Self::END,
      TokenKind::EXCEPTION => Self::EXCEPTION,
      TokenKind::FN => Self::FN,
      TokenKind::FUN => Self::FUN,
      TokenKind::HANDLE => Self::HANDLE,
      TokenKind::IF => Self::IF,
      TokenKind::IN => Self::IN,
      TokenKind::INFIX => Self::INFIX,
      TokenKind::INFIXR => Self::INFIXR,
      TokenKind::LET => Self::LET,
      TokenKind::LOCAL => Self::LOCAL,
      TokenKind::NONFIX => Self::NONFIX,
      TokenKind::OF => Self::OF,
      TokenKind::OP => Self::OP,
      TokenKind::OPEN => Self::OPEN,
      TokenKind::ORELSE => Self::ORELSE,
      TokenKind::RAISE => Self::RAISE,
      TokenKind::REC => Self::REC,
      TokenKind::THEN => Self::THEN,
      TokenKind::TYPE => Self::TYPE,
      TokenKind::VAL => Self::VAL,
      TokenKind::WITH => Self::WITH,
      TokenKind::WITHTYPE => Self::WITHTYPE,
      TokenKind::WHILE => Self::WHILE,

      // module reserved words
      TokenKind::EQTYPE => Self::EQTYPE,
      TokenKind::FUNCTOR => Self::FUNCTOR,
      TokenKind::INCLUDE => Self::INCLUDE,
      TokenKind::SHARING => Self::SHARING,
      TokenKind::SIG => Self::SIG,
      TokenKind::SIGNATURE => Self::SIGNATURE,
      TokenKind::STRUCT => Self::STRUCT,
      TokenKind::STRUCTURE => Self::STRUCTURE,
      TokenKind::WHERE => Self::WHERE,

      // misc symbols
      TokenKind::LPAREN => Self::LPAREN,
      TokenKind::RPAREN => Self::RPAREN,
      TokenKind::LSQUARE => Self::LSQUARE,
      TokenKind::RSQUARE => Self::RSQUARE,
      TokenKind::LCURLY => Self::LCURLY,
      TokenKind::RCURLY => Self::RCURLY,
      TokenKind::COMMA => Self::COMMA,
      TokenKind::COLON => Self::COLON,
      TokenKind::SEMICOLON => Self::SEMICOLON,
      TokenKind::DOTDOTDOT => Self::DOTDOTDOT,
      TokenKind::UNDERSCORE => Self::UNDERSCORE,
      TokenKind::BAR => Self::BAR,
      TokenKind::EQUAL => Self::EQUAL,
      TokenKind::DARROW => Self::DARROW,
      TokenKind::ARROW => Self::ARROW,
      TokenKind::POUND => Self::POUND,
      TokenKind::COLONGT => Self::COLONGT,
      TokenKind::DOT => Self::DOT,

      // special constants
      TokenKind::DECINT => Self::DECINT,
      TokenKind::HEXINT => Self::HEXINT,
      TokenKind::DECWORD => Self::DECWORD,
      TokenKind::HEXWORD => Self::HEXWORD,
      TokenKind::REAL => Self::REAL,
      TokenKind::STRING => Self::STRING,
      TokenKind::CHAR => Self::CHAR,

      // identifiers
      TokenKind::TYVARID => Self::TYVARID,
      TokenKind::IDENT(..) => Self::IDENT,

      // non-token tokens
      TokenKind::COMMENT => Self::COMMENT,
      TokenKind::WHITESPACE => Self::WHITESPACE,
      TokenKind::EOF => Self::EOF,
      TokenKind::ERROR => Self::ERROR,
    }
  }
}
