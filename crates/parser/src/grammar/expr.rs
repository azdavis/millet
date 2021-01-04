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
  let mut lhs = lhs(p)?;

  loop {
    let (left_bp, right_bp) = match p.at_op() {
      Ok(&OpInfo(l, r)) => (l, r),
      Err(()) => break,
    };

    if left_bp < min_bp {
      break;
    }

    p.bump();

    let parsed_rhs = expr_bp(p, right_bp).is_some();
    lhs = lhs.precede(p).complete(p, SyntaxKind::ERROR);

    if !parsed_rhs {
      break;
    }
  }

  Some(lhs)
}

fn lhs(p: &mut Parser) -> Option<CompletedMarker> {
  let cm = if p.at_set(&SPECIAL_CONSTANTS) {
    scon(p)
  } else if p.at(TokenKind::IF) {
    conditional(p)
  } else if p.at(TokenKind::FN) {
    lambda(p)
  } else if p.at(TokenKind::CASE) {
    case(p)
  } else if p.at(TokenKind::WHILE) {
    while_loop(p)
  } else {
    p.error();
    return None;
  };

  Some(cm)
}

fn scon(p: &mut Parser) -> CompletedMarker {
  debug_assert!(p.at_set(&SPECIAL_CONSTANTS));

  let m = p.start();
  p.bump();
  m.complete(p, SyntaxKind::SCON)
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
  m.complete(p, todo!())
}

fn lambda(p: &mut Parser) -> CompletedMarker {
  debug_assert!(p.at(TokenKind::FN));

  let m = p.start();
  p.bump();
  pat_match(p);
  m.complete(p, todo!())
}

fn case(p: &mut Parser) -> CompletedMarker {
  debug_assert!(p.at(TokenKind::CASE));

  let m = p.start();
  p.bump();
  expr(p);
  p.expect(TokenKind::OF);
  pat_match(p);
  m.complete(p, todo!())
}

fn while_loop(p: &mut Parser) -> CompletedMarker {
  debug_assert!(p.at(TokenKind::WHILE));

  let m = p.start();
  p.bump();
  expr(p);
  p.expect(TokenKind::DO);
  expr(p);
  m.complete(p, todo!())
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
