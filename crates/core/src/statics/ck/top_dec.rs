//! Check top-level declarations.

use crate::ast::{StrDec, TopDec};
use crate::intern::StrRef;
use crate::loc::Located;
use crate::statics::ck::dec;
use crate::statics::types::{Basis, Cx, Env, Error, Result, State, TyVarSet};

pub fn ck(mut bs: Basis, st: &mut State, top_dec: &Located<TopDec<StrRef>>) -> Result<Basis> {
  match &top_dec.val {
    TopDec::StrDec(str_dec) => {
      let env = ck_str_dec(&bs, st, str_dec)?;
      bs.o_plus(env);
      Ok(bs)
    }
    TopDec::SigDec(_) => Err(top_dec.loc.wrap(Error::Todo)),
    TopDec::FunDec(_) => Err(top_dec.loc.wrap(Error::Todo)),
  }
}

fn ck_str_dec(bs: &Basis, st: &mut State, str_dec: &Located<StrDec<StrRef>>) -> Result<Env> {
  match &str_dec.val {
    StrDec::Dec(dec) => {
      let cx = Cx {
        ty_names: bs.ty_names.clone(),
        ty_vars: TyVarSet::new(),
        env: bs.env.clone(),
      };
      dec::ck(&cx, st, dec)
    }
    StrDec::Structure(_) => Err(str_dec.loc.wrap(Error::Todo)),
    StrDec::Local(_, _) => Err(str_dec.loc.wrap(Error::Todo)),
    StrDec::Seq(_) => Err(str_dec.loc.wrap(Error::Todo)),
  }
}
