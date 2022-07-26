use crate::common::{get_name, get_path};
use crate::pat::tuple;
use crate::util::{Cx, ErrorKind};
use crate::{exp, pat, ty};
use syntax::ast::{self, AstNode as _, AstPtr};

pub(crate) fn get(cx: &mut Cx, dec: Option<ast::Dec>) -> hir::DecIdx {
  let dec = dec?;
  let mut decs: Vec<_> = dec
    .dec_in_seqs()
    .map(|x| get_one(cx, x.dec_one()?))
    .collect();
  if decs.len() == 1 {
    decs.pop().unwrap()
  } else {
    cx.dec(hir::Dec::Seq(decs), AstPtr::new(&dec))
  }
}

pub(crate) fn get_one(cx: &mut Cx, dec: ast::DecOne) -> hir::DecIdx {
  let ptr = AstPtr::new(&dec);
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
      if let Some(bar) = dec.bar() {
        cx.err(bar.text_range(), ErrorKind::PrecedingBar);
      }
      let ty_vars = ty::var_seq(dec.ty_var_seq());
      let val_binds: Vec<_> = dec
        .fun_binds()
        .map(|fun_bind| {
          let mut name = None::<syntax::SyntaxToken>;
          let mut num_pats = None::<usize>;
          let mut exp_ptr = None::<AstPtr<ast::Exp>>;
          let mut pat_ptr = None::<AstPtr<ast::Pat>>;
          let arms: Vec<_> = fun_bind
            .fun_bind_cases()
            .map(|case| {
              let mut pats = Vec::<hir::PatIdx>::with_capacity(1);
              let head_name = case
                .fun_bind_case_head()
                .and_then(|head| match head {
                  ast::FunBindCaseHead::PrefixFunBindCaseHead(head) => head.name_star_eq(),
                  ast::FunBindCaseHead::InfixFunBindCaseHead(head) => {
                    let lhs = head.lhs();
                    let rhs = head.rhs();
                    if pat_ptr.is_none() {
                      pat_ptr = lhs.as_ref().or(rhs.as_ref()).map(AstPtr::new);
                    }
                    if let Some(ref ptr) = pat_ptr {
                      let tup = tuple([pat::get(cx, lhs), pat::get(cx, rhs)]);
                      pats.push(cx.pat(tup, ptr.clone()));
                    }
                    head.name_star_eq()
                  }
                })
                .map(|x| x.token);
              match (name.as_ref(), head_name) {
                (_, None) => {}
                (None, Some(head_name)) => name = Some(head_name),
                (Some(name), Some(head_name)) => {
                  if name.text() != head_name.text() {
                    cx.err(
                      head_name.text_range(),
                      ErrorKind::FunBindMismatchedName(
                        name.text().to_owned(),
                        head_name.text().to_owned(),
                      ),
                    );
                  }
                }
              }
              for pat in case.pats() {
                if pat_ptr.is_none() {
                  pat_ptr = Some(AstPtr::new(&pat));
                }
                pats.push(pat::get(cx, Some(pat)));
              }
              match num_pats {
                None => num_pats = Some(pats.len()),
                Some(num_pats) => {
                  if num_pats != pats.len() {
                    cx.err(
                      case.syntax().text_range(),
                      ErrorKind::FunBindWrongNumPats(num_pats, pats.len()),
                    );
                  }
                }
              }
              let pat = pat_ptr.clone().and_then(|ptr| {
                if pats.len() == 1 {
                  pats.pop().unwrap()
                } else {
                  cx.pat(pat::tuple(pats), ptr)
                }
              });
              let ty = case.ty_annotation().map(|x| ty::get(cx, x.ty()));
              let exp = case.exp().and_then(|exp| {
                let ptr = AstPtr::new(&exp);
                if exp_ptr.is_none() {
                  exp_ptr = Some(ptr.clone());
                }
                let mut exp = exp::get(cx, Some(exp));
                if let Some(ty) = ty {
                  exp = cx.exp(hir::Exp::Typed(exp, ty), ptr);
                }
                exp
              });
              (pat, exp)
            })
            .collect();
          // not the greatest, since we have no body at all if the ptrs are None. but if they were
          // both None, then something's very strange about the fun_bind_cases anyway.
          let exp = exp_ptr.zip(pat_ptr.clone()).and_then(|(exp_ptr, pat_ptr)| {
            let arg_names: Vec<_> = (0..num_pats.unwrap_or(1)).map(|_| cx.fresh()).collect();
            let mut arg_exprs = arg_names
              .iter()
              .map(|name| cx.exp(exp::name(name.as_str()), exp_ptr.clone()));
            let head = if arg_exprs.len() == 1 {
              arg_exprs.next().unwrap()
            } else {
              let tup = exp::tuple(arg_exprs);
              cx.exp(tup, exp_ptr.clone())
            };
            let case = exp::case(cx, head, arms, exp_ptr.clone());
            arg_names
              .into_iter()
              .rev()
              .fold(cx.exp(case, exp_ptr.clone()), |body, name| {
                let pat = cx.pat(pat::name(name.as_str()), pat_ptr.clone());
                cx.exp(hir::Exp::Fn(vec![(pat, body)]), exp_ptr.clone())
              })
          });
          hir::ValBind {
            rec: true,
            pat: name
              .zip(pat_ptr)
              .and_then(|(tok, pat_ptr)| cx.pat(pat::name(tok.text()), pat_ptr)),
            exp,
          }
        })
        .collect();
      hir::Dec::Val(ty_vars, val_binds)
    }
    ast::DecOne::TyDec(dec) => hir::Dec::Ty(ty_binds(cx, dec.ty_binds())),
    ast::DecOne::DatDec(dec) => {
      let dbs: Vec<_> = dat_binds(cx, dec.dat_binds()).collect();
      let tbs = ty_binds(cx, dec.with_type().into_iter().flat_map(|x| x.ty_binds()));
      hir::Dec::Datatype(dbs, tbs)
    }
    ast::DecOne::DatCopyDec(dec) => {
      hir::Dec::DatatypeCopy(get_name(dec.name())?, get_path(dec.path()?)?)
    }
    ast::DecOne::AbstypeDec(dec) => {
      let dbs: Vec<_> = dat_binds(cx, dec.dat_binds()).collect();
      let tbs = ty_binds(cx, dec.with_type().into_iter().flat_map(|x| x.ty_binds()));
      let inner = get(cx, dec.dec());
      hir::Dec::Abstype(dbs, tbs, inner)
    }
    ast::DecOne::ExDec(dec) => hir::Dec::Exception(
      dec
        .ex_binds()
        .filter_map(|ex_bind| {
          let name = hir::Name::new(ex_bind.name_star_eq()?.token.text());
          let ret = match ex_bind.ex_bind_inner() {
            None => hir::ExBind::New(name, None),
            Some(ast::ExBindInner::OfTy(x)) => hir::ExBind::New(name, Some(ty::get(cx, x.ty()))),
            Some(ast::ExBindInner::EqPath(x)) => hir::ExBind::Copy(name, get_path(x.path()?)?),
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
    ast::DecOne::DoDec(ref inner) => {
      // emit an error, but lower anyway.
      cx.err(
        dec.syntax().text_range(),
        ErrorKind::Unsupported("`do` declarations"),
      );
      hir::Dec::Val(
        Vec::new(),
        vec![hir::ValBind {
          rec: false,
          pat: cx.pat(pat::tuple([]), ptr.clone()),
          exp: exp::get(cx, inner.exp()),
        }],
      )
    }
  };
  cx.dec(ret, ptr)
}

pub(crate) fn dat_binds<'a, I>(cx: &'a mut Cx, iter: I) -> impl Iterator<Item = hir::DatBind> + 'a
where
  I: Iterator<Item = ast::DatBind> + 'a,
{
  iter.filter_map(|dat_bind| {
    if let Some(bar) = dat_bind.bar() {
      cx.err(bar.text_range(), ErrorKind::PrecedingBar);
    }
    Some(hir::DatBind {
      ty_vars: ty::var_seq(dat_bind.ty_var_seq()),
      name: get_name(dat_bind.name())?,
      cons: dat_bind
        .con_binds()
        .filter_map(|con_bind| {
          Some(hir::ConBind {
            name: hir::Name::new(con_bind.name_star_eq()?.token.text()),
            ty: con_bind.of_ty().map(|x| ty::get(cx, x.ty())),
          })
        })
        .collect(),
    })
  })
}

fn ty_binds<I>(cx: &mut Cx, iter: I) -> Vec<hir::TyBind>
where
  I: Iterator<Item = ast::TyBind>,
{
  iter
    .filter_map(|ty_bind| {
      let name = get_name(ty_bind.name())?;
      Some(hir::TyBind {
        ty_vars: ty::var_seq(ty_bind.ty_var_seq()),
        name,
        ty: ty::get(cx, ty_bind.ty()),
      })
    })
    .collect()
}
