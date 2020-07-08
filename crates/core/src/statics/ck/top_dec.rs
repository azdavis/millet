//! Check top-level declarations.

use crate::ast::{StrDec, TopDec};
use crate::intern::StrRef;
use crate::loc::Located;
use crate::statics::ck::dec;
use crate::statics::types::{Basis, Cx, Error, Result, State, TyVarSet};

pub fn ck(bs: Basis, st: &mut State, top_dec: &Located<TopDec<StrRef>>) -> Result<Basis> {
  match &top_dec.val {
    TopDec::StrDec(str_dec) => match &str_dec.val {
      StrDec::Dec(dec) => {
        let mut cx = Cx {
          ty_names: bs.ty_names,
          ty_vars: TyVarSet::new(),
          env: bs.env,
        };
        cx.o_plus(dec::ck(&cx, st, dec)?);
        Ok(Basis {
          env: cx.env,
          ty_names: cx.ty_names,
          ..bs
        })
      }
      StrDec::Structure(_) => Err(top_dec.loc.wrap(Error::Todo)),
      StrDec::Local(_, _) => Err(top_dec.loc.wrap(Error::Todo)),
      StrDec::Seq(_) => Err(top_dec.loc.wrap(Error::Todo)),
    },
    TopDec::SigDec(_) => Err(top_dec.loc.wrap(Error::Todo)),
    TopDec::FunDec(_) => Err(top_dec.loc.wrap(Error::Todo)),
  }
}
