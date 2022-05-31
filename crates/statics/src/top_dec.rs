use crate::dec;
use crate::error::ErrorKind;
use crate::st::St;
use crate::types::{Bs, Env};

pub(crate) fn get(st: &mut St, bs: &mut Bs, ars: &hir::Arenas, top_dec: hir::TopDecIdx) {
  match ars.top_dec[top_dec] {
    hir::TopDec::Str(str_dec) => {
      let mut env = Env::default();
      get_str_dec(st, bs, ars, &mut env, str_dec);
      bs.env.extend(env);
    }
    hir::TopDec::Sig(_) => st.err(top_dec, ErrorKind::Unsupported),
    hir::TopDec::Functor(_) => st.err(top_dec, ErrorKind::Unsupported),
  }
}

fn get_str_dec(st: &mut St, bs: &Bs, ars: &hir::Arenas, env: &mut Env, str_dec: hir::StrDecIdx) {
  let str_dec = match str_dec {
    Some(x) => x,
    None => return,
  };
  match &ars.str_dec[str_dec] {
    hir::StrDec::Dec(dec) => dec::get(st, &bs.as_cx(), ars, env, *dec),
    hir::StrDec::Structure(_) => st.err(str_dec, ErrorKind::Unsupported),
    hir::StrDec::Local(local_dec, in_dec) => {
      let mut local_env = Env::default();
      get_str_dec(st, bs, ars, &mut local_env, *local_dec);
      let mut bs = bs.clone();
      bs.env.extend(local_env);
      get_str_dec(st, &bs, ars, env, *in_dec);
    }
    hir::StrDec::Seq(str_decs) => {
      let mut bs = bs.clone();
      for &str_dec in str_decs {
        let mut one_env = Env::default();
        get_str_dec(st, &bs, ars, &mut one_env, str_dec);
        bs.env.extend(one_env.clone());
        env.extend(one_env);
      }
    }
  }
}
