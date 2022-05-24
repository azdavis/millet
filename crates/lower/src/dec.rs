use crate::common::{get_name, get_path};
use crate::util::Cx;
use crate::{exp, pat, ty};
use syntax::ast;

pub(crate) fn get(cx: &mut Cx, dec: Option<ast::Dec>) -> hir::DecIdx {
  let mut decs: Vec<_> = dec
    .into_iter()
    .flat_map(|x| x.dec_in_seqs())
    .filter_map(|x| {
      let res = get_one(cx, x.dec_one()?)?;
      Some(cx.arenas.dec.alloc(res))
    })
    .collect();
  if decs.len() == 1 {
    decs.pop().unwrap()
  } else {
    cx.arenas.dec.alloc(hir::Dec::Seq(decs))
  }
}

pub(crate) fn get_one(cx: &mut Cx, dec: ast::DecOne) -> Option<hir::Dec> {
  let ret = match dec {
    ast::DecOne::ValDec(dec) => hir::Dec::Val(
      ty::var_seq(dec.ty_var_seq()),
      dec
        .val_binds()
        .map(|val_bind| hir::ValBind {
          rec: val_bind.rec_kw().is_some(),
          pat: pat::get(cx, val_bind.pat()),
          exp: exp::get(cx, val_bind.exp()),
        })
        .collect(),
    ),
    ast::DecOne::FunDec(dec) => {
      let ty_vars = ty::var_seq(dec.ty_var_seq());
      let val_binds: Vec<_> = dec
        .fun_binds()
        .map(|fun_bind| {
          let mut name = None::<syntax::SyntaxToken>;
          let mut num_pats = None::<usize>;
          let arms: Vec<_> = fun_bind
            .fun_bind_cases()
            .map(|case| {
              let mut pats = Vec::<hir::PatIdx>::with_capacity(2);
              let head_name = case
                .fun_bind_case_head()
                .and_then(|head| match head {
                  ast::FunBindCaseHead::PrefixFunBindCaseHead(head) => head.name_plus(),
                  ast::FunBindCaseHead::InfixFunBindCaseHead(head) => {
                    pats.push(pat::get(cx, head.lhs()));
                    pats.push(pat::get(cx, head.rhs()));
                    head.name_plus()
                  }
                })
                .map(|x| x.token);
              match (name.as_ref(), head_name) {
                (_, None) => {}
                (None, Some(head_name)) => name = Some(head_name),
                (Some(name), Some(head_name)) => {
                  if name.text() != head_name.text() {
                    // TODO error
                  }
                }
              }
              pats.extend(case.pats().map(|pat| pat::get(cx, Some(pat))));
              match num_pats {
                None => num_pats = Some(pats.len()),
                Some(num_pats) => {
                  if num_pats != pats.len() {
                    // TODO error
                  }
                }
              }
              let pat = if pats.len() == 1 {
                pats.pop().unwrap()
              } else {
                cx.arenas.pat.alloc(pat::tuple(pats))
              };
              let ty = case.ty_annotation().map(|x| ty::get(cx, x.ty()));
              let mut exp = exp::get(cx, case.exp());
              if let Some(ty) = ty {
                exp = cx.arenas.exp.alloc(hir::Exp::Typed(exp, ty));
              }
              (pat, exp)
            })
            .collect();
          let pat = name.map_or(hir::Pat::None, |tok| pat::name(tok.text()));
          let arg_names: Vec<_> = (0..num_pats.unwrap_or(1)).map(|_| cx.fresh()).collect();
          let mut arg_exprs = arg_names
            .iter()
            .map(|name| cx.arenas.exp.alloc(exp::name(name.as_str())));
          let head = if arg_exprs.len() == 1 {
            arg_exprs.next().unwrap()
          } else {
            let tup = exp::tuple(arg_exprs);
            cx.arenas.exp.alloc(tup)
          };
          let case = exp::case(cx, head, arms);
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
    ast::DecOne::TyDec(dec) => ty_binds(cx, dec.ty_binds()),
    ast::DecOne::DatDec(dec) => {
      let mut ret = hir::Dec::Datatype(dat_binds(cx, dec.dat_binds()));
      if let Some(with_ty) = dec.with_type() {
        let ty_dec = ty_binds(cx, with_ty.ty_binds());
        ret = hir::Dec::Seq(vec![cx.arenas.dec.alloc(ret), cx.arenas.dec.alloc(ty_dec)]);
      }
      ret
    }
    ast::DecOne::DatCopyDec(dec) => {
      hir::Dec::DatatypeCopy(get_name(dec.name())?, get_path(dec.path()?)?)
    }
    ast::DecOne::AbstypeDec(dec) => {
      let dbs = dat_binds(cx, dec.dat_binds());
      let ty_dec = dec.with_type().map(|x| ty_binds(cx, x.ty_binds()));
      let mut d = get(cx, dec.dec());
      if let Some(ty_dec) = ty_dec {
        let ty_dec = cx.arenas.dec.alloc(ty_dec);
        d = cx.arenas.dec.alloc(hir::Dec::Seq(vec![d, ty_dec]));
      }
      // TODO: "see note in text"
      hir::Dec::Abstype(dbs, d)
    }
    ast::DecOne::ExDec(dec) => hir::Dec::Exception(
      dec
        .ex_binds()
        .filter_map(|ex_bind| {
          let name = hir::Name::new(ex_bind.name_plus()?.token.text());
          let ret = match ex_bind.ex_bind_inner()? {
            ast::ExBindInner::OfTy(x) => {
              hir::ExBind::New(name, x.ty().map(|x| ty::get(cx, Some(x))))
            }
            ast::ExBindInner::EqPath(x) => hir::ExBind::Copy(name, get_path(x.path()?)?),
          };
          Some(ret)
        })
        .collect(),
    ),
    ast::DecOne::LocalDec(dec) => hir::Dec::Local(get(cx, dec.local_dec()), get(cx, dec.in_dec())),
    ast::DecOne::OpenDec(dec) => hir::Dec::Open(dec.paths().filter_map(get_path).collect()),
    ast::DecOne::InfixDec(_) | ast::DecOne::InfixrDec(_) | ast::DecOne::NonfixDec(_) => {
      return None
    }
  };
  Some(ret)
}

pub(crate) fn dat_binds<I>(cx: &mut Cx, iter: I) -> Vec<hir::DatBind>
where
  I: Iterator<Item = ast::DatBind>,
{
  iter
    .filter_map(|dat_bind| {
      Some(hir::DatBind {
        ty_vars: ty::var_seq(dat_bind.ty_var_seq()),
        name: get_name(dat_bind.name())?,
        cons: dat_bind
          .con_binds()
          .filter_map(|con_bind| {
            Some(hir::ConBind {
              name: hir::Name::new(con_bind.name_plus()?.token.text()),
              ty: con_bind.of_ty().map(|x| ty::get(cx, x.ty())),
            })
          })
          .collect(),
      })
    })
    .collect()
}

fn ty_binds<I>(cx: &mut Cx, iter: I) -> hir::Dec
where
  I: Iterator<Item = ast::TyBind>,
{
  hir::Dec::Ty(
    iter
      .filter_map(|ty_bind| {
        let name = get_name(ty_bind.name())?;
        Some(hir::TyBind {
          ty_vars: ty::var_seq(ty_bind.ty_var_seq()),
          name,
          ty: ty::get(cx, ty_bind.ty()),
        })
      })
      .collect(),
  )
}
