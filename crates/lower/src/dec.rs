use crate::util::Cx;
use crate::{exp, pat, ty};
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
      let ty_vars = ty_var_seq(dec.ty_var_seq());
      let binds: Vec<_> = dec
        .val_binds()
        .map(|x| hir::ValBind {
          rec: x.rec_kw().is_some(),
          pat: pat::get(cx, x.pat()),
          exp: exp::get(cx, x.exp()),
        })
        .collect();
      hir::Dec::Val(ty_vars, binds)
    }
    ast::Dec::FunDec(dec) => {
      let ty_vars = ty_var_seq(dec.ty_var_seq());
      let val_binds: Vec<_> = dec
        .fun_binds()
        .map(|fun_bind| {
          let mut name = None::<syntax::SyntaxToken>;
          let mut num_pats = None::<usize>;
          let arms: Vec<_> = fun_bind
            .fun_bind_cases()
            .map(|case| {
              let mut pats = Vec::<hir::PatIdx>::with_capacity(2);
              let head_name = case.fun_bind_case_head().and_then(|head| match head {
                ast::FunBindCaseHead::PrefixFunBindCaseHead(head) => head.name(),
                ast::FunBindCaseHead::InfixFunBindCaseHead(head) => {
                  pats.push(pat::get(cx, head.lhs()));
                  pats.push(pat::get(cx, head.rhs()));
                  head.name()
                }
              });
              match (name.as_ref(), head_name) {
                (_, None) => {}
                (None, Some(head_name)) => name = Some(head_name),
                (Some(name), Some(head_name)) => {
                  if name.text() != head_name.text() {
                    // TODO error
                  }
                }
              }
              pats.extend(case.pats().map(|x| pat::get(cx, Some(x))));
              match num_pats {
                None => num_pats = Some(pats.len()),
                Some(num_pats) => {
                  if num_pats != pats.len() {
                    // TODO error
                  }
                }
              }
              let pat = cx.arenas.pat.alloc(pat::tuple(pats));
              let ty = case.ty_annotation().map(|x| ty::get(cx, x.ty()));
              let mut exp = exp::get(cx, case.exp());
              if let Some(ty) = ty {
                exp = cx.arenas.exp.alloc(hir::Exp::Typed(exp, ty));
              }
              (pat, exp)
            })
            .collect();
          let pat = name.map_or(hir::Pat::None, |x| pat::name(x.text()));
          let arg_names: Vec<_> = (0..num_pats.unwrap_or(1)).map(|_| cx.fresh()).collect();
          let head = exp::tuple(
            arg_names
              .iter()
              .map(|x| cx.arenas.exp.alloc(exp::name(x.as_str()))),
          );
          let head = cx.arenas.exp.alloc(head);
          let case = exp::case_exp(cx, head, arms);
          hir::ValBind {
            rec: true,
            pat: cx.arenas.pat.alloc(pat),
            exp: arg_names
              .into_iter()
              .rev()
              .fold(cx.arenas.exp.alloc(case), |body, name| {
                let pat = cx.arenas.pat.alloc(pat::name(name.as_str()));
                cx.arenas.exp.alloc(hir::Exp::Fn(vec![(pat, body)]))
              }),
          }
        })
        .collect();
      hir::Dec::Val(ty_vars, val_binds)
    }
    ast::Dec::TyDec(dec) => hir::Dec::Ty(
      dec
        .ty_binds()
        .filter_map(|x| {
          let name = hir::Name::new(x.name()?.text());
          Some(hir::TyBind {
            ty_vars: ty_var_seq(x.ty_var_seq()),
            name,
            ty: ty::get(cx, x.ty()),
          })
        })
        .collect(),
    ),
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

fn ty_var_seq(tvs: Option<ast::TyVarSeq>) -> Vec<hir::TyVar> {
  tvs
    .into_iter()
    .flat_map(|x| x.ty_var_args())
    .filter_map(|x| x.ty_var())
    .map(|tok| hir::TyVar::new(tok.text()))
    .collect()
}
