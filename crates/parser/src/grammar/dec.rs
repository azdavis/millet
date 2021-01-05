use super::*;

pub(super) fn top_dec(p: &mut Parser) -> Option<CompletedMarker> {
  if p.at(TokenKind::SIGNATURE) {
    sig_dec(p)
  } else if p.at(TokenKind::FUNCTOR) {
    functor(p)
  } else {
    todo!()
  }
}

fn sig_dec(p: &mut Parser) -> Option<CompletedMarker> {
  debug_assert!(p.at(TokenKind::SIGNATURE));

  let m = p.start();
  p.bump();
  loop {
    p.expect_set(&IDENT);
    p.expect(TokenKind::EQUAL);
    sig_exp(p)?;
    if p.at(TokenKind::AND) {
      p.bump();
    } else {
      break;
    }
  }
  Some(m.complete(p, SyntaxKind::TOPDEC_SIG))
}

fn sig_exp(p: &mut Parser) -> Option<CompletedMarker> {
  let m = p.start();

  let cm = if p.at(TokenKind::SIG) {
    p.bump();
    spec(p);
    p.expect(TokenKind::END);
    m.complete(p, SyntaxKind::SIGEXP_SIG)
  } else if p.at_set(&IDENT) {
    p.bump();
    m.complete(p, SyntaxKind::SIGEXP_ID)
  } else {
    p.error();
    return None;
  };

  while p.at(TokenKind::WHERE) {
    let m = cm.precede(p);
    p.bump();
    ty::ty_var_seq(p);
    long_id(p);
    p.expect(TokenKind::EQUAL);
    ty::ty(p);
    cm = m.complete(p, SyntaxKind::SIGEXP_WHERE);
  }

  Some(cm)
}

fn spec(p: &mut Parser) -> Option<CompletedMarker> {
  todo!()
}
