mod token_kind;
pub use token_kind::TokenKind;

use logos::Logos;
use text_size::{TextRange, TextSize};

use std::convert::TryFrom;

pub struct Lexer<'a> {
  inner: logos::Lexer<'a, TokenKind>,
}

impl<'a> Lexer<'a> {
  pub fn new(input: &'a str) -> Self {
    Self {
      inner: TokenKind::lexer(input),
    }
  }
}

impl<'a> Iterator for Lexer<'a> {
  type Item = Token<'a>;

  fn next(&mut self) -> Option<Self::Item> {
    let kind = self.inner.next()?;
    let text = self.inner.slice();

    let range = {
      let std::ops::Range { start, end } = self.inner.span();
      let start = TextSize::try_from(start).unwrap();
      let end = TextSize::try_from(end).unwrap();

      TextRange::new(start, end)
    };

    Some(Self::Item { kind, text, range })
  }
}

#[derive(Debug, PartialEq)]
pub struct Token<'a> {
  pub kind: TokenKind,
  pub text: &'a str,
  pub range: TextRange,
}

#[cfg(test)]
mod tests {
  use super::*;

  fn check(input: &str, kind: TokenKind) {
    let mut lexer = Lexer::new(input);

    let token = lexer.next().unwrap();
    assert_eq!(token.kind, kind);
    assert_eq!(token.text, input);
  }

  #[test]
  fn lex_abstype() {
    check("abstype", TokenKind::ABSTYPE)
  }

  #[test]
  fn lex_and() {
    check("and", TokenKind::AND)
  }

  #[test]
  fn lex_andalso() {
    check("andalso", TokenKind::ANDALSO)
  }

  #[test]
  fn lex_as() {
    check("as", TokenKind::AS)
  }

  #[test]
  fn lex_case() {
    check("case", TokenKind::CASE)
  }

  #[test]
  fn lex_datatype() {
    check("datatype", TokenKind::DATATYPE)
  }

  #[test]
  fn lex_do() {
    check("do", TokenKind::DO)
  }

  #[test]
  fn lex_else() {
    check("else", TokenKind::ELSE)
  }

  #[test]
  fn lex_end() {
    check("end", TokenKind::END)
  }

  #[test]
  fn lex_exception() {
    check("exception", TokenKind::EXCEPTION)
  }

  #[test]
  fn lex_fn() {
    check("fn", TokenKind::FN)
  }

  #[test]
  fn lex_fun() {
    check("fun", TokenKind::FUN)
  }

  #[test]
  fn lex_handle() {
    check("handle", TokenKind::HANDLE)
  }

  #[test]
  fn lex_if() {
    check("if", TokenKind::IF)
  }

  #[test]
  fn lex_in() {
    check("in", TokenKind::IN)
  }

  #[test]
  fn lex_infix() {
    check("infix", TokenKind::INFIX)
  }

  #[test]
  fn lex_infixr() {
    check("infixr", TokenKind::INFIXR)
  }

  #[test]
  fn lex_let() {
    check("let", TokenKind::LET)
  }

  #[test]
  fn lex_local() {
    check("local", TokenKind::LOCAL)
  }

  #[test]
  fn lex_nonfix() {
    check("nonfix", TokenKind::NONFIX)
  }

  #[test]
  fn lex_of() {
    check("of", TokenKind::OF)
  }

  #[test]
  fn lex_op() {
    check("op", TokenKind::OP)
  }

  #[test]
  fn lex_open() {
    check("open", TokenKind::OPEN)
  }

  #[test]
  fn lex_orelse() {
    check("orelse", TokenKind::ORELSE)
  }

  #[test]
  fn lex_raise() {
    check("raise", TokenKind::RAISE)
  }

  #[test]
  fn lex_rec() {
    check("rec", TokenKind::REC)
  }

  #[test]
  fn lex_then() {
    check("then", TokenKind::THEN)
  }

  #[test]
  fn lex_type() {
    check("type", TokenKind::TYPE)
  }

  #[test]
  fn lex_val() {
    check("val", TokenKind::VAL)
  }

  #[test]
  fn lex_with() {
    check("with", TokenKind::WITH)
  }

  #[test]
  fn lex_withtype() {
    check("withtype", TokenKind::WITHTYPE)
  }

  #[test]
  fn lex_while() {
    check("while", TokenKind::WHILE)
  }

  #[test]
  fn lex_eqtype() {
    check("eqtype", TokenKind::EQTYPE)
  }

  #[test]
  fn lex_functor() {
    check("functor", TokenKind::FUNCTOR)
  }

  #[test]
  fn lex_include() {
    check("include", TokenKind::INCLUDE)
  }

  #[test]
  fn lex_sharing() {
    check("sharing", TokenKind::SHARING)
  }

  #[test]
  fn lex_sig() {
    check("sig", TokenKind::SIG)
  }

  #[test]
  fn lex_signature() {
    check("signature", TokenKind::SIGNATURE)
  }

  #[test]
  fn lex_struct() {
    check("struct", TokenKind::STRUCT)
  }

  #[test]
  fn lex_structure() {
    check("structure", TokenKind::STRUCTURE)
  }

  #[test]
  fn lex_where() {
    check("where", TokenKind::WHERE)
  }

  #[test]
  fn lex_lparen() {
    check("(", TokenKind::LPAREN)
  }

  #[test]
  fn lex_rparen() {
    check(")", TokenKind::RPAREN)
  }

  #[test]
  fn lex_lsquare() {
    check("[", TokenKind::LSQUARE)
  }

  #[test]
  fn lex_rsquare() {
    check("]", TokenKind::RSQUARE)
  }

  #[test]
  fn lex_lcurly() {
    check("{", TokenKind::LCURLY)
  }

  #[test]
  fn lex_rcurly() {
    check("}", TokenKind::RCURLY)
  }

  #[test]
  fn lex_comma() {
    check(",", TokenKind::COMMA)
  }

  #[test]
  fn lex_colon() {
    check(":", TokenKind::COLON)
  }

  #[test]
  fn lex_semicolon() {
    check(";", TokenKind::SEMICOLON)
  }

  #[test]
  fn lex_dotdotdot() {
    check("...", TokenKind::DOTDOTDOT)
  }

  #[test]
  fn lex_underscore() {
    check("_", TokenKind::UNDERSCORE)
  }

  #[test]
  fn lex_bar() {
    check("|", TokenKind::BAR)
  }

  #[test]
  fn lex_equal() {
    check("=", TokenKind::EQUAL)
  }

  #[test]
  fn lex_darrow() {
    check("=>", TokenKind::DARROW)
  }

  #[test]
  fn lex_arrow() {
    check("->", TokenKind::ARROW)
  }

  #[test]
  fn lex_pound() {
    check("#", TokenKind::POUND)
  }

  #[test]
  fn lex_colongt() {
    check(":>", TokenKind::COLONGT)
  }

  #[test]
  fn lex_dot() {
    check(".", TokenKind::DOT)
  }

  #[test]
  fn lex_decint() {
    check("417651", TokenKind::DECINT)
  }

  #[test]
  fn lex_negative_decint() {
    check("~341842", TokenKind::DECINT)
  }

  #[test]
  fn lex_hexint() {
    check("0x41AfbC", TokenKind::HEXINT)
  }

  #[test]
  fn lex_negative_hexint() {
    check("~0xC0FfeE", TokenKind::HEXINT)
  }

  #[test]
  fn lex_decword() {
    check("0w417651", TokenKind::DECWORD)
  }

  #[test]
  fn lex_negative_decword() {
    check("~0w341842", TokenKind::DECWORD)
  }

  #[test]
  fn lex_hexword() {
    check("0xw41AfbC", TokenKind::HEXWORD)
  }

  #[test]
  fn lex_negative_hexword() {
    check("~0xwC0FfeE", TokenKind::HEXWORD)
  }

  #[test]
  fn lex_real() {
    check("3.14", TokenKind::REAL)
  }

  #[test]
  fn lex_real_exponent() {
    check("6.022e~23", TokenKind::REAL)
  }

  #[test]
  fn lex_string_literal() {
    check(r#""this is a string""#, TokenKind::STRING)
  }

  #[test]
  fn lex_string_escapes() {
    check(
      r#"\a\b\n\v\f\r\t\^C\127\u0412\"\\\             \some chars"#,
      TokenKind::STRING,
    )
  }

  #[test]
  fn lex_char() {
    check("#\"A\"", TokenKind::CHAR)
  }

  #[test]
  fn lex_char_escape() {
    check(r##"#"\n""##, TokenKind::CHAR)
  }

  #[test]
  fn lex_tyvar() {
    check("'a", TokenKind::TYVARID)
  }

  #[test]
  fn lex_tyvar_eq() {
    check("''equality", TokenKind::TYVARID)
  }

  #[test]
  fn lex_ident() {
    check("foobar", TokenKind::IDENT)
  }

  #[test]
  fn lex_symbolic_ident() {
    check("<|>", TokenKind::IDENT)
  }

  #[test]
  fn lex_comment() {
    check("(* this is a comment *)", TokenKind::COMMENT)
  }

  #[test]
  fn lex_nested_comment() {
    check(
      "(* this is a (* multi-level (* nested *) comment *) *)",
      TokenKind::COMMENT,
    )
  }
  // WHITESPACE,
}
