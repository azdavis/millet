use super::*;

const SPECIAL_CONSTANTS: [TokenKind; 7] = [
  TokenKind::DECINT,
  TokenKind::HEXINT,
  TokenKind::DECWORD,
  TokenKind::HEXWORD,
  TokenKind::REAL,
  TokenKind::STRING,
  TokenKind::CHAR,
];

pub(super) fn expr(p: &mut Parser) -> Option<CompletedMarker> {
  expr_bp(p, 0)
}

fn expr_bp(p: &mut Parser, min_bp: u8) -> Option<CompletedMarker> {
  let cm = if p.at(TokenKind::CASE) {
    case(p)
  } else if p.at(TokenKind::FN) {
    lambda(p)
  } else if p.at(TokenKind::IF) {
    conditional(p)
  } else if p.at(TokenKind::RAISE) {
    raise(p)
  } else if p.at(TokenKind::WHILE) {
    while_loop(p)
  } else {
    let mut exp = atexp(p)?;
    todo!()
  };

  todo!()
}

fn atexp(p: &mut Parser) -> Option<CompletedMarker> {
  let m = p.start();

  let cm = if p.at_set(&SPECIAL_CONSTANTS) {
    p.bump();
    m.complete(p, SyntaxKind::SCON)
  } else if p.at(TokenKind::OP) {
    p.bump();
    todo!()
  } else if p.at(TokenKind::LCURLY) {
    todo!()
  } else if p.at(TokenKind::POUND) {
    todo!()
  } else if p.at(TokenKind::LPAREN) {
    todo!()
  } else if p.at(TokenKind::LSQUARE) {
    todo!()
  } else if p.at(TokenKind::LET) {
    todo!()
  } else if p.at_set(&IDENT) {
    todo!()
  } else if p.at(TokenKind::EQUAL) {
    todo!()
  } else {
    todo!() // Original returns Some(None); forget if this is an error or not
  };

  Some(cm)
}

fn conditional(p: &mut Parser) -> CompletedMarker {
  debug_assert!(p.at(TokenKind::IF));

  let m = p.start();
  p.bump();
  expr(p);
  p.expect(TokenKind::THEN);
  expr(p);
  p.expect(TokenKind::ELSE);
  expr(p);
  m.complete(p, SyntaxKind::EXP_IF)
}

fn lambda(p: &mut Parser) -> CompletedMarker {
  debug_assert!(p.at(TokenKind::FN));

  let m = p.start();
  p.bump();
  pat_match(p);
  m.complete(p, SyntaxKind::EXP_FN)
}

fn case(p: &mut Parser) -> CompletedMarker {
  debug_assert!(p.at(TokenKind::CASE));

  let m = p.start();
  p.bump();
  expr(p);
  p.expect(TokenKind::OF);
  pat_match(p);
  m.complete(p, SyntaxKind::EXP_CASE)
}

fn raise(p: &mut Parser) -> CompletedMarker {
  debug_assert!(p.at(TokenKind::RAISE));

  let m = p.start();
  p.bump();
  expr(p);
  m.complete(p, SyntaxKind::EXP_RAISE)
}

fn while_loop(p: &mut Parser) -> CompletedMarker {
  debug_assert!(p.at(TokenKind::WHILE));

  let m = p.start();
  p.bump();
  expr(p);
  p.expect(TokenKind::DO);
  expr(p);
  m.complete(p, SyntaxKind::EXP_WHILE)
}

fn pat_match(p: &mut Parser) -> CompletedMarker {
  todo!()
}

#[cfg(test)]
mod test {
  use crate::check;
  use expect_test::expect;

  #[test]
  fn int_literal() {
    check(
      "123",
      expect![[r#"
        ROOT@0..3
          SCON@0..3
            DECINT@0..3 "123""#]],
    );
  }
}
