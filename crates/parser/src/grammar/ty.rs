use super::*;

pub(super) fn ty(p: &mut Parser) -> Option<CompletedMarker> {
  todo!()
}

pub(super) fn ty_var_seq(p: &mut Parser) -> Option<CompletedMarker> {
  p.expect(TokenKind::LPAREN);
  p.expect(TokenKind::TYVARID);
  loop {
    if p.at(TokenKind::COMMA){
      p.bump();
      p.expect(TokenKind::TYVARID);
    } else {
      break;
    }
  }
  p.expect(TokenKind::RParen);
}
