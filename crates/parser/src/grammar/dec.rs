use super::*;

pub(super) fn long_id(p: &mut Parser) -> Option<CompletedMarker> {
  debug_assert!(p.at_set(&IDENT));
  let m = p.start();

  p.bump();

  loop {
    if p.at(TokenKind::DOT) {
      p.bump();
      p.expect_set(&IDENT);
    } else {
      break;
    }
  }

  Some(m.complete(p, SyntaxKind::LONG_ID))
}
pub(super) fn top_dec(p: &mut Parser) -> Option<CompletedMarker> {
  if p.at(TokenKind::SIGNATURE) {
    sig_dec(p)
  } else if p.at(TokenKind::FUNCTOR) {
    fun_dec(p)
  } else {
    str_dec(p)
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
    spec(p)?;
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

fn spec_ind(p: &mut Parser) -> bool {
  p.at(TokenKind::VAL)
    || p.at(TokenKind::TYPE)
    || p.at(TokenKind::EQTYPE)
    || p.at(TokenKind::DATATYPE)
    || p.at(TokenKind::EXCEPTION)
    || p.at(TokenKind::STRUCTURE)
    || p.at(TokenKind::INCLUDE)
}

fn spec(p: &mut Parser) -> Option<CompletedMarker> {
  // outer should be None in the case where we are not accumulating a SPEC_SEQ, and Some(m)
  // otherwise
  let mut outer: Option<CompletedMarker> = None;

  let cm = loop {
    // parse a single regular spec
    let inner = if p.at(TokenKind::VAL) {
      spec_val(p)
    } else if p.at(TokenKind::TYPE) {
      spec_type(p)
    } else if p.at(TokenKind::EQTYPE) {
      spec_eqtype(p)
    } else if p.at(TokenKind::DATATYPE) {
      let m = p.start();
      p.bump();

      if p.at_set(&IDENT) {
        p.bump();
        p.expect(TokenKind::EQUAL);

        if p.at(TokenKind::DATATYPE) {
          // datatype copy case
          p.bump();
          long_id(p);
          m.complete(p, SyntaxKind::SPEC_DATATYPE_COPY)
        } else {
          // datdesc case
          loop {
            con_desc(p);
            if p.at(TokenKind::BAR) {
              p.bump();
            } else {
              break;
            }
          }

          loop {
            if p.at(TokenKind::AND) {
              p.bump();
              dat_desc(p);
            } else {
              break;
            }
          }
          m.complete(p, SyntaxKind::DATATYPE)
        }
      } else {
        // datdesc case
        loop {
          dat_desc(p);
          if p.at(TokenKind::AND) {
            p.bump();
          } else {
            break;
          }
        }
        m.complete(p, SyntaxKind::SPEC_DATATYPE)
      }
    } else if p.at(TokenKind::EXCEPTION) {
      spec_ex(p)
    } else if p.at(TokenKind::STRUCTURE) {
      spec_struct(p)
    } else if p.at(TokenKind::INCLUDE) {
      spec_include(p)
    } else if p.at(TokenKind::END) {
      let m = p.start();
      return Some(m.complete(p, SyntaxKind::SPEC_EMPTY));
    } else {
      p.error();
      None
    };

    // get rid of optional semicolon
    if p.at(TokenKind::SEMICOLON) {
      p.bump();
    }

    // if we reach the end
    if p.at(TokenKind::END) {
      break inner;
    }

    // if we are not accumulating a seq already and we have a spec next
    if outer.is_none() && spec_ind(p) {
      outer = Some(inner.precede(p));
    }

    // collapse the seq we are accumulating if it exists
    if p.at(TokenKind::SHARING) {
      match outer {
        None => inner = inner.precede(p),
        Some(m) => {
          let outer_complete = m.complete(p, SyntaxKind::SPEC_SEQ);
          outer = None;
          inner = outer_complete.precede(p);
        }
      }
      p.bump();
      p.expect(TokenKind::TYPE);
      p.expect_set(&IDENT);
      loop {
        p.expect(TokenKind::EQUAL);
        p.expect_set(&IDENT);
        if !p.at(TokenKind::EQUAL) {
          break;
        }
      }
      inner.complete(p, SyntaxKind::SPEC_SHARING);
    }
  };

  match outer {
    None => Some(cm),
    Some(m) => Some(m.complete(p, SyntaxKind::SPEC_SEQ)),
  }
}

fn spec_val(p: &mut Parser) -> Option<CompletedMarker> {
  debug_assert!(p.at(TokenKind::VAL));
  let m = p.start();

  p.expect(TokenKind::VAL);

  loop {
    val_desc(p);
    if p.at(TokenKind::AND) {
      p.bump();
    } else {
      break;
    }
  }

  Some(m.complete(p, SyntaxKind::SPEC_VAL))
}

fn val_desc(p: &mut Parser) -> Option<CompletedMarker> {
  let m = p.start();

  p.expect_set(&IDENT);
  p.expect(TokenKind::COLON);
  ty::ty(p);

  Some(m.complete(p, SyntaxKind::VALDESC))
}

fn spec_type(p: &mut Parser) -> Option<CompletedMarker> {
  debug_assert!(p.at(TokenKind::TYPE));
  let m = p.start();

  p.expect(TokenKind::TYPE);

  loop {
    type_desc(p);
    if p.at(TokenKind::AND) {
      p.bump();
    } else {
      break;
    }
  }

  Some(m.complete(p, SyntaxKind::SPEC_TYPE))
}

fn spec_eqtype(p: &mut Parser) -> Option<CompletedMarker> {
  debug_assert!(p.at(TokenKind::EQTYPE));
  let m = p.start();

  p.expect(TokenKind::EQTYPE);

  loop {
    type_desc(p);
    if p.at(TokenKind::AND) {
      p.bump();
    } else {
      break;
    }
  }

  Some(m.complete(p, SyntaxKind::SPEC_EQTYPE))
}

fn type_desc(p: &mut Parser) -> Option<CompletedMarker> {
  let m = p.start();

  ty::ty_var_seq(p);
  p.expect_set(&IDENT);

  Some(m.complete(p, SyntaxKind::TYPDESC))
}

fn dat_desc(p: &mut Parser) -> Option<CompletedMarker> {
  let m = p.start();

  ty::ty_var_seq(p);
  p.expect_set(&IDENT);
  p.expect(TokenKind::EQUAL);
  loop {
    con_desc(p);
    if p.at(TokenKind::BAR) {
      p.bump();
    } else {
      break;
    }
  }

  Some(m.complete(p, SyntaxKind::DATDESC))
}

fn con_desc(p: &mut Parser) -> Option<CompletedMarker> {
  let m = p.start();

  p.expect_set(&IDENT);

  if p.at(TokenKind::OF) {
    p.bump();
    ty::ty(p);
  }

  Some(m.complete(p, SyntaxKind::CONDESC))
}

fn spec_ex(p: &mut Parser) -> Option<CompletedMarker> {
  debug_assert!(p.at(TokenKind::EXCEPTION));
  let m = p.start();

  p.expect(TokenKind::EXCEPTION);

  loop {
    ex_desc(p);
    if p.at(TokenKind::AND) {
      p.bump();
    } else {
      break;
    }
  }

  Some(m.complete(p, SyntaxKind::SPEC_EXCEPTION))
}

fn ex_desc(p: &mut Parser) -> Option<CompletedMarker> {
  let m = p.start();

  p.expect_set(&IDENT);

  if p.at(TokenKind::OF) {
    p.bump();
    ty::ty(p);
  }

  Some(m.complete(p, SyntaxKind::EXDESC))
}

fn spec_struct(p: &mut Parser) -> Option<CompletedMarker> {
  debug_assert!(p.at(TokenKind::STRUCTURE));
  let m = p.start();

  p.expect(TokenKind::STRUCTURE);

  loop {
    str_desc(p);
    if p.at(TokenKind::AND) {
      p.bump();
    } else {
      break;
    }
  }

  Some(m.complete(p, SyntaxKind::SPEC_STRUCTURE))
}

fn str_desc(p: &mut Parser) -> Option<CompletedMarker> {
  let m = p.start();

  p.expect_set(&IDENT);
  p.expect(TokenKind::COLON);
  sig_exp(p);

  Some(m.complete(p, SyntaxKind::EXDESC))
}

fn spec_include(p: &mut Parser) -> Option<CompletedMarker> {
  debug_assert!(p.at(TokenKind::INCLUDE));
  let m = p.start();

  sig_exp(p);

  Some(m.complete(p, SyntaxKind::SPEC_INCLUDE))
}

fn fun_dec(p: &mut Parser) -> Option<CompletedMarker> {
  debug_assert!(p.at(TokenKind::FUNCTOR));
  let m = p.start();
  p.bump();

  loop {
    p.expect_set(&IDENT);
    p.expect(TokenKind::LPAREN);
    p.expect_set(&IDENT);
    p.expect(TokenKind::COLON);
    sig_exp(p)?;
    p.expect(TokenKind::RPAREN);
    p.expect(TokenKind::EQUAL);
    str_exp(p);
    if p.at(TokenKind::AND) {
      p.bump();
    } else {
      break;
    }
  }
}

fn str_exp(p: &mut Parser) -> Option<CompletedMarker> {
  let m = p.start();

  let cm = if p.at(TokenKind::STRUCT) {
    p.bump();
    str_dec(p)?;
    p.expect(TokenKind::END);
    m.complete(p, SyntaxKind::STREXP_STRUCT)
  } else if p.at(TokenKind::LET) {
    p.bump();
    str_dec(p)?;
    p.expect(TokenKind::IN);
    str_exp(p)?;
    p.expect(TokenKind::END);
    m.complete(p, SyntaxKind::STREXP_LET)
  } else if p.at_set(&IDENT) {
    // it could be one of four things...
    p.bump();
    if p.at(TokenKind::DOT) {
      loop {
        if p.at(TokenKind::DOT) {
          p.bump();
          p.expect_set(&IDENT);
        } else {
          break;
        }
      }
      m.complete(p, SyntaxKind::STREXP_LONGSTRID)
    } else if p.at(TokenKind::COLON) {
      p.bump();
      sig_exp(p)?;
      m.complete(p, SyntaxKind::STREXP_COLON)
    } else if p.at(TokenKind::COLONGT) {
      p.bump();
      sig_exp(p)?;
      m.complete(p, SyntaxKind::STREXP_COLONGT)
    } else if p.at(TokenKind::LPAREN) {
      p.bump();
      str_exp(p)?;
      p.expect(TokenKind::RPAREN);
      m.complete(p, SyntaxKind::STREXP_FUNID)
    } else {
      m.complete(p, SyntaxKind::STREXP_LONGSTRID)
    }
  };

  Some(cm)
}

fn dec_ind_no_local(p: &mut Parser) -> bool {
  p.at(TokenKind::VAL)
    || p.at(TokenKind::TYPE)
    || p.at(TokenKind::DATATYPE)
    || p.at(TokenKind::ABSTYPE)
    || p.at(TokenKind::EXCEPTION)
    || p.at(TokenKind::OPEN)
    || p.at(TokenKind::INFIX)
    || p.at(TokenKind::INFIXR)
    || p.at(TokenKind::NONFIX)
}
fn str_dec(p: &mut Parser) -> Option<CompletedMarker> {
  let m = p.start();
  let mut outer = None;

  let cm = if p.at(TokenKind::STRUCTURE) {
    p.bump();
    loop {
      p.expect_set(&IDENT);
      p.expect(TokenKind::EQUAL);
      str_exp(p);
      if p.at(TokenKind::AND) {
        p.bump();
      } else {
        break;
      }
    }
    m.complete(p, SyntaxKind::STRDEC_STRUCT)
  } else if p.at(TokenKind::LOCAL) {
    // this is ambiguous, it could be a dec...
    p.bump();

    let mut frames = Vec::new();
    frames.push(m);
    let mut flag = false;

    // loop to get new frames as long as we see "local"
    loop {
      if p.at(TokenKind::LOCAL) {
        let new = p.start();
        p.bump();
        frames.push(new)
      } else if dec_ind_no_local(p) {
        flag = true;
      } else if p.at(TokenKind::STRUCTURE) {
        flag = false;
      } else {
        p.error();
      }
    }

    // while there are still frames left
    while !frames.is_empty() {
      let new_m = frames.pop(); // the most recent frame
      if flag { // its a dec
        dec(p);
        p.expect(TokenKind::IN);
        dec(p);
        p.expect(TokenKind::END);
        new_m.complete(SyntaxKind::DEC_LOCAL)
      } else { // strdec
        str_dec(p);
        p.expect(TokenKind::IN);
        str_dec(p);
        p.expect(TokenKind::END);
        new_m.complete(p, SyntaxKind::STRDEC_LOCAL)
      }
    }

    if dec_ind_no_local(p) {
      dec(p);
      p.expect(TokenKind::IN);
      dec(p);
      p.expect(TokenKind::END);
    } else if p.at(TokenKind::STRUCTURE) {
    } else if p.at(TokenKind::LOCAL) {
    }
    str_dec(p)?;
    p.expect(TokenKind::IN);
    str_dec(p)?;
    p.expect(TokenKind::END);
    m.complete(p, SyntaxKind::STRDEC_LOCAL)
  } else {
    todo!()
  };

  Some(cm)
}
