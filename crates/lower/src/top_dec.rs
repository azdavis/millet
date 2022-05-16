// TODO rm
#![allow(unused_variables)]

use crate::common::get_name;
use crate::dec;
use crate::util::Cx;
use syntax::ast;

pub(crate) fn get(cx: &mut Cx, top_dec: ast::TopDec) -> Option<hir::TopDec> {
  let ret = match top_dec {
    ast::TopDec::StrDecTopDec(top_dec) => hir::TopDec::Str(get_str_dec(cx, top_dec.str_dec())),
    ast::TopDec::SigDec(_) => todo!(),
    ast::TopDec::FunctorDec(_) => todo!(),
  };
  Some(ret)
}

fn get_str_dec(cx: &mut Cx, str_dec: Option<ast::StrDec>) -> hir::StrDecIdx {
  todo!()
}

fn get_str_dec_one(cx: &mut Cx, str_dec: ast::StrDecOne) -> hir::StrDec {
  match str_dec {
    ast::StrDecOne::DecStrDec(str_dec) => hir::StrDec::Dec(dec::get(cx, str_dec.dec())),
    ast::StrDecOne::StructureStrDec(str_dec) => hir::StrDec::Structure(
      str_dec
        .str_binds()
        .filter_map(|str_bind| {
          Some(hir::StrBind {
            name: get_name(str_bind.name())?,
            str_exp: get_str_exp(cx, str_bind.str_exp()),
          })
        })
        .collect(),
    ),
    ast::StrDecOne::LocalStrDec(str_dec) => hir::StrDec::Local(
      get_str_dec(cx, str_dec.local_dec()),
      get_str_dec(cx, str_dec.in_dec()),
    ),
  }
}

fn get_str_exp(cx: &mut Cx, str_exp: Option<ast::StrExp>) -> hir::StrExpIdx {
  todo!()
}

fn get_str_exp_(cx: &mut Cx, str_exp: ast::StrExp) -> Option<hir::StrExp> {
  todo!()
}
