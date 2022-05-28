use crate::dec;
use crate::st::St;
use crate::types::{Cx, Env};

pub(crate) fn get(st: &mut St, cx: &mut Cx, ars: &hir::Arenas, top_dec: &hir::TopDec) {
  match top_dec {
    hir::TopDec::Str(str_dec) => get_str_dec(st, cx, ars, *str_dec),
    hir::TopDec::Sig(_) => {
      // TODO
    }
    hir::TopDec::Functor(_) => {
      // TODO
    }
  }
}

pub(crate) fn get_str_dec(st: &mut St, cx: &mut Cx, ars: &hir::Arenas, str_dec: hir::StrDecIdx) {
  let str_dec = match str_dec {
    Some(x) => x,
    None => return,
  };
  match &ars.str_dec[str_dec] {
    hir::StrDec::Dec(dec) => {
      let mut env = Env::default();
      dec::get(st, cx, ars, &mut env, *dec);
      cx.env.extend(env);
    }
    hir::StrDec::Structure(_) => {
      // TODO
    }
    hir::StrDec::Local(_, _) => {
      // TODO
    }
    hir::StrDec::Seq(str_decs) => {
      for &str_dec in str_decs {
        get_str_dec(st, cx, ars, str_dec);
      }
    }
  }
}
