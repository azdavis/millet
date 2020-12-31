use logos::{Lexer, Logos};

use std::fmt;

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash, Logos)]
pub enum TokenKind {
  // core reserved words
  #[token("abstype")]
  ABSTYPE,
  #[token("and")]
  AND,
  #[token("andalso")]
  ANDALSO,
  #[token("as")]
  AS,
  #[token("case")]
  CASE,
  #[token("datatype")]
  DATATYPE,
  #[token("do")]
  DO,
  #[token("else")]
  ELSE,
  #[token("end")]
  END,
  #[token("exception")]
  EXCEPTION,
  #[token("fn")]
  FN,
  #[token("fun")]
  FUN,
  #[token("handle")]
  HANDLE,
  #[token("if")]
  IF,
  #[token("in")]
  IN,
  #[token("infix")]
  INFIX,
  #[token("infixr")]
  INFIXR,
  #[token("let")]
  LET,
  #[token("local")]
  LOCAL,
  #[token("nonfix")]
  NONFIX,
  #[token("of")]
  OF,
  #[token("op")]
  OP,
  #[token("open")]
  OPEN,
  #[token("orelse")]
  ORELSE,
  #[token("raise")]
  RAISE,
  #[token("rec")]
  REC,
  #[token("then")]
  THEN,
  #[token("type")]
  TYPE,
  #[token("val")]
  VAL,
  #[token("with")]
  WITH,
  #[token("withtype")]
  WITHTYPE,
  #[token("while")]
  WHILE,

  // module reserved words
  #[token("eqtype")]
  EQTYPE,
  #[token("functor")]
  FUNCTOR,
  #[token("include")]
  INCLUDE,
  #[token("sharing")]
  SHARING,
  #[token("sig")]
  SIG,
  #[token("signature")]
  SIGNATURE,
  #[token("struct")]
  STRUCT,
  #[token("structure")]
  STRUCTURE,
  #[token("where")]
  WHERE,

  // misc symbols
  #[token("(")]
  LPAREN,
  #[token(")")]
  RPAREN,
  #[token("[")]
  LSQUARE,
  #[token("]")]
  RSQUARE,
  #[token("{")]
  LCURLY,
  #[token("}")]
  RCURLY,
  #[token(",")]
  COMMA,
  #[token(":")]
  COLON,
  #[token(";")]
  SEMICOLON,
  #[token("...")]
  DOTDOTDOT,
  #[token("_")]
  UNDERSCORE,
  #[token("|")]
  BAR,
  #[token("=")]
  EQUAL,
  #[token("=>")]
  DARROW,
  #[token("->")]
  ARROW,
  #[token("#")]
  POUND,
  #[token(":>")]
  COLONGT,
  #[token(".")]
  DOT,

  // special constants
  #[regex("~?[0-9]+")]
  DECINT,
  #[regex("~?0x[a-fA-F0-0]+")]
  HEXINT,
  #[regex("~?0w[0-9]+")]
  DECWORD,
  #[regex("~?0wx[a-fA-F0-0]+")]
  HEXWORD,
  #[regex(r"~?[0-9]+\.[0-9]+")]
  #[regex(r"~?[0-9]+(\.[0-9]+)?e~?[0-9]+")]
  REAL,
  #[regex(r#""[^"]*""#)] // TODO: implement escape sequences properly
  STRING,
  #[regex("#\".\"")] // ditto
  CHAR,

  // identifiers
  TYVARID,
  IDENT,

  // non-token tokens
  #[token("(*", comment)]
  COMMENT,
  #[regex("[ \n\r\t]+")]
  WHITESPACE,
  EOF,
  #[error]
  ERROR,
}

fn comment(lex: &mut Lexer<TokenKind>) -> bool {
  let mut level = 0;
  loop {
    let s = lex.slice();
    match &s[s.len() - 2..] {
      "(*" => {
        level += 1;
        if lex.remainder().len() >= 2 {
          lex.bump(2)
        } else {
          lex.bump(lex.remainder().len());
          return false;
        }
      }
      "*)" => {
        level -= 1;
        if level == 0 {
          return true;
        } else if lex.remainder().len() >= 2 {
          lex.bump(2)
        } else {
          lex.bump(lex.remainder().len());
          return false;
        }
      }
      _ => lex.bump(1),
    }
  }
}

impl fmt::Display for TokenKind {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    f.write_str(match self {
      // core reserved words
      TokenKind::ABSTYPE => "'abstype'",
      TokenKind::AND => "'and'",
      TokenKind::ANDALSO => "'andalso'",
      TokenKind::AS => "'as'",
      TokenKind::CASE => "'case'",
      TokenKind::DATATYPE => "'datatype'",
      TokenKind::DO => "'do'",
      TokenKind::ELSE => "'else'",
      TokenKind::END => "'end'",
      TokenKind::EXCEPTION => "'exception'",
      TokenKind::FN => "'fn'",
      TokenKind::FUN => "'fun'",
      TokenKind::HANDLE => "'handle'",
      TokenKind::IF => "'if'",
      TokenKind::IN => "'in'",
      TokenKind::INFIX => "'infix'",
      TokenKind::INFIXR => "'infixr'",
      TokenKind::LET => "'let'",
      TokenKind::LOCAL => "'local'",
      TokenKind::NONFIX => "'nonfix'",
      TokenKind::OF => "'of'",
      TokenKind::OP => "'op'",
      TokenKind::OPEN => "'open'",
      TokenKind::ORELSE => "'orelse'",
      TokenKind::RAISE => "'raise'",
      TokenKind::REC => "'rec'",
      TokenKind::THEN => "'then'",
      TokenKind::TYPE => "'type'",
      TokenKind::VAL => "'val'",
      TokenKind::WITH => "'with'",
      TokenKind::WITHTYPE => "'withtype'",
      TokenKind::WHILE => "'while'",

      // module reserved words
      TokenKind::EQTYPE => "'eqtype'",
      TokenKind::FUNCTOR => "'functor'",
      TokenKind::INCLUDE => "'include'",
      TokenKind::SHARING => "'sharing'",
      TokenKind::SIG => "'sig'",
      TokenKind::SIGNATURE => "'signature'",
      TokenKind::STRUCT => "'struct'",
      TokenKind::STRUCTURE => "'structure'",
      TokenKind::WHERE => "'where'",

      // misc symbol
      TokenKind::LPAREN => "'('",
      TokenKind::RPAREN => "')'",
      TokenKind::LSQUARE => "'['",
      TokenKind::RSQUARE => "']'",
      TokenKind::LCURLY => "'{'",
      TokenKind::RCURLY => "'}'",
      TokenKind::COMMA => "','",
      TokenKind::COLON => "':'",
      TokenKind::SEMICOLON => "';'",
      TokenKind::DOTDOTDOT => "'...'",
      TokenKind::UNDERSCORE => "'_'",
      TokenKind::BAR => "'|'",
      TokenKind::EQUAL => "'='",
      TokenKind::DARROW => "'=>'",
      TokenKind::ARROW => "'->'",
      TokenKind::POUND => "'#'",
      TokenKind::COLONGT => "':>'",
      TokenKind::DOT => "'.'",

      // special constant
      TokenKind::DECINT => "decimal integer literal",
      TokenKind::HEXINT => "hex integer literal",
      TokenKind::DECWORD => "decimal word literal",
      TokenKind::HEXWORD => "hex word literal",
      TokenKind::REAL => "real literal",
      TokenKind::STRING => "string literal",
      TokenKind::CHAR => "character literal",

      // identifiers
      TokenKind::TYVARID => "type variable identifier",
      TokenKind::IDENT => "identifier",

      // non-token tokens
      TokenKind::WHITESPACE => "whitespace",
      TokenKind::COMMENT => "a comment",
      TokenKind::EOF => "end of file",
      TokenKind::ERROR => "an unknown token",
    })
  }
}
