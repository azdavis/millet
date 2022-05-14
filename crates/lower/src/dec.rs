use crate::util::Cx;
use crate::{exp, pat};
use syntax::ast;

pub(crate) fn get(cx: &mut Cx, dec: Option<ast::DecSeq>) -> Vec<hir::DecIdx> {
  dec
    .into_iter()
    .flat_map(|x| x.dec_in_seqs())
    .filter_map(|x| x.dec())
    .map(|dec| {
      let res = get_one(cx, dec);
      cx.arenas.dec.alloc(res)
    })
    .collect()
}

fn get_one(cx: &mut Cx, dec: ast::Dec) -> hir::Dec {
  match dec {
    ast::Dec::ValDec(dec) => {
      let vars: Vec<_> = dec
        .ty_var_seq()
        .into_iter()
        .flat_map(|x| x.ty_var_args())
        .filter_map(|x| x.ty_var())
        .map(|x| hir::TyVar::new(x.text()))
        .collect();
      let binds: Vec<_> = dec
        .val_binds()
        .map(|x| hir::ValBind {
          rec: x.rec_kw().is_some(),
          pat: pat::get(cx, x.pat()),
          exp: exp::get(cx, x.exp()),
        })
        .collect();
      hir::Dec::Val(vars, binds)
    }
    ast::Dec::FunDec(_) => todo!(),
    ast::Dec::TyDec(_) => todo!(),
    ast::Dec::DatDec(_) => todo!(),
    ast::Dec::DatCopyDec(_) => todo!(),
    ast::Dec::AbstypeDec(_) => todo!(),
    ast::Dec::ExDec(_) => todo!(),
    ast::Dec::LocalDec(_) => todo!(),
    ast::Dec::OpenDec(_) => todo!(),
    ast::Dec::InfixDec(_) => todo!(),
    ast::Dec::InfixrDec(_) => todo!(),
    ast::Dec::NonfixDec(_) => todo!(),
  }
}
