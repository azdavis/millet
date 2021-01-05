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
  #[regex("~?0x[a-fA-F0-9]+")]
  HEXINT,
  #[regex("~?0w[0-9]+")]
  DECWORD,
  #[regex("~?0wx[a-fA-F0-9]+")]
  HEXWORD,
  #[regex(r"~?[0-9]+\.[0-9]+")]
  #[regex(r"~?[0-9]+(\.[0-9]+)?e~?[0-9]+")]
  REAL,
  #[regex("\"[^\"]*\"")]
  STRING,
  #[regex("#\".\"")]
  CHAR,

  // identifiers
  #[regex("'[0-9a-zA-Z'_]*")] // unclear if this is 0 or more (as PolyML uses) or 1+ (SML/NJ)
  TYVARID,
  #[regex("[A-Za-z][0-9a-zA-Z'_]*", |_| IdentType::Alphanumeric)]
  #[regex("[!%&$#+-/:<=>?@\\~`^|*]+", |_| IdentType::Symbolic)]
  IDENT(IdentType),

  // non-token tokens
  #[token("(*", comment)]
  COMMENT,
  #[regex("[ \n\r\t]+")]
  WHITESPACE,
  EOF,
  #[error]
  ERROR,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub enum IdentType {
  Alphanumeric,
  Symbolic,
}

impl TokenKind {
  pub fn is_trivia(self) -> bool {
    matches!(self, Self::WHITESPACE | Self::COMMENT)
  }
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

fn _str_lit(_lex: &mut Lexer<TokenKind>) -> bool {
  // if b == b'"' {
  //   self.i += 1;
  //   let mut str_bs = Vec::new();
  //   while let Some(&b) = self.bs.get(self.i) {
  //     match b {
  //       b'\n' => return Err(Error::UnclosedStringConstant),
  //       b'"' => {
  //         self.i += 1;
  //         return if is_char {
  //           if str_bs.len() == 1 {
  //             let b = str_bs.pop().unwrap();
  //             Ok(Token::Char(b))
  //           } else {
  //             Err(Error::InvalidCharConstant)
  //           }
  //         } else {
  //           str_bs.shrink_to_fit();
  //           let string = String::from_utf8(str_bs).unwrap();
  //           let str_ref = self.store.insert(string.into());
  //           Ok(Token::String(str_ref))
  //         };
  //       }
  //       b'\\' => {
  //         self.i += 1;
  //         let b = match self.bs.get(self.i) {
  //           None => return Err(Error::UnclosedStringConstant),
  //           Some(x) => *x,
  //         };
  //         match b {
  //           b'a' => str_bs.push(7),
  //           b'b' => str_bs.push(8),
  //           b't' => str_bs.push(9),
  //           b'n' => str_bs.push(10),
  //           b'v' => str_bs.push(11),
  //           b'f' => str_bs.push(12),
  //           b'r' => str_bs.push(13),
  //           b'^' => {
  //             self.i += 1;
  //             let b = match self.bs.get(self.i) {
  //               None => return Err(Error::UnclosedStringConstant),
  //               Some(x) => *x,
  //             };
  //             str_bs.push(b - 64);
  //           }
  //           b'u' => {
  //             if self.i + 4 >= self.bs.len() {
  //               return Err(Error::UnclosedStringConstant);
  //             }
  //             match (
  //               hex(self.bs[self.i + 1]),
  //               hex(self.bs[self.i + 2]),
  //               hex(self.bs[self.i + 3]),
  //               hex(self.bs[self.i + 4]),
  //             ) {
  //               (Some(0), Some(0), Some(d1), Some(d2)) => {
  //                 str_bs.push(d1 * 16 + d2);
  //                 self.i += 4;
  //               }
  //               _ => return Err(Error::InvalidStringConstant),
  //             }
  //           }
  //           b'"' => str_bs.push(b'"'),
  //           b'\\' => str_bs.push(b'\\'),
  //           b => {
  //             if let Some(d1) = dec(b) {
  //               if self.i + 2 >= self.bs.len() {
  //                 return Err(Error::UnclosedStringConstant);
  //               }
  //               match (dec(self.bs[self.i + 1]), dec(self.bs[self.i + 2])) {
  //                 (Some(d2), Some(d3)) => {
  //                   str_bs.push((d1 * 10 + d2) * 10 + d3);
  //                   self.i += 2;
  //                 }
  //                 _ => return Err(Error::InvalidStringConstant),
  //               }
  //             } else if is_formatting(b) {
  //               loop {
  //                 self.i += 1;
  //                 let b = match self.bs.get(self.i) {
  //                   None => return Err(Error::UnclosedStringConstant),
  //                   Some(x) => *x,
  //                 };
  //                 if b == b'\\' {
  //                   break;
  //                 }
  //                 if !is_formatting(b) {
  //                   return Err(Error::InvalidStringConstant);
  //                 }
  //               }
  //             } else {
  //               return Err(Error::InvalidStringConstant);
  //             }
  //           }
  //         }
  //       }
  //       b => str_bs.push(b),
  //     }
  //     self.i += 1;
  //   }
  //   return Err(Error::UnclosedStringConstant);
  // }
  todo!()
}

fn _char_lit(_lex: &mut Lexer<TokenKind>) -> bool {
  todo!()
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
      TokenKind::IDENT(IdentType::Alphanumeric) => "alphanumeric identifier",
      TokenKind::IDENT(IdentType::Symbolic) => "symbolic identifier",

      // non-token tokens
      TokenKind::WHITESPACE => "whitespace",
      TokenKind::COMMENT => "a comment",
      TokenKind::EOF => "end of file",
      TokenKind::ERROR => "an unknown token",
    })
  }
}
