use crate::dec;
use crate::error::ErrorKind;
use crate::st::St;
use crate::types::{Cx, Env};

pub(crate) fn get(st: &mut St, cx: &Cx, ars: &hir::Arenas, top_dec: &hir::TopDec) {
  match top_dec {
    hir::TopDec::Str(str_dec) => get_str_dec(st, cx, ars, *str_dec),
    hir::TopDec::Sig(_) => st.err(ErrorKind::Unimplemented),
    hir::TopDec::Functor(_) => st.err(ErrorKind::Unimplemented),
  }
}
pub(crate) fn get_str_dec(st: &mut St, cx: &Cx, ars: &hir::Arenas, str_dec: hir::StrDecIdx) {
  match &ars.str_dec[str_dec] {
    hir::StrDec::Dec(dec) => {
      let mut env = Env::default();
      dec::get(st, cx, ars, &mut env, *dec)
    }
    hir::StrDec::Structure(_) => st.err(ErrorKind::Unimplemented),
    hir::StrDec::Local(_, _) => st.err(ErrorKind::Unimplemented),
    hir::StrDec::Seq(str_decs) => {
      for &str_dec in str_decs {
        get_str_dec(st, cx, ars, str_dec);
      }
    }
  }
}
