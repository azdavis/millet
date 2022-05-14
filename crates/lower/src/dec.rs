use crate::util::Cx;
use crate::{exp, pat};
use syntax::ast;

pub(crate) fn get(cx: &mut Cx, dec: Option<ast::Dec>) -> Vec<hir::DecIdx> {
  dec
    .into_iter()
    .flat_map(|x| x.dec_in_seqs())
    .filter_map(|x| x.dec_one())
    .map(|dec| {
      let res = get_one(cx, dec);
      cx.arenas.dec.alloc(res)
    })
    .collect()
}

fn get_one(cx: &mut Cx, dec: ast::DecOne) -> hir::Dec {
  match dec {
    ast::DecOne::ValDec(dec) => {
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
    ast::DecOne::FunDec(_) => todo!(),
    ast::DecOne::TyDec(_) => todo!(),
    ast::DecOne::DatDec(_) => todo!(),
    ast::DecOne::DatCopyDec(_) => todo!(),
    ast::DecOne::AbstypeDec(_) => todo!(),
    ast::DecOne::ExDec(_) => todo!(),
    ast::DecOne::LocalDec(_) => todo!(),
    ast::DecOne::OpenDec(_) => todo!(),
    ast::DecOne::InfixDec(_) => todo!(),
    ast::DecOne::InfixrDec(_) => todo!(),
    ast::DecOne::NonfixDec(_) => todo!(),
  }
}
