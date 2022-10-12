use crate::common::{get_name, get_path};
use crate::pat::tuple;
use crate::util::{Cx, ErrorKind};
use crate::{exp, pat, ty};
use sml_syntax::ast::{self, AstNode as _, SyntaxNodePtr};

fn get_dec_flavor<T, G, S>(cx: &mut Cx, dec: Option<ast::Dec>, g: G, s: S) -> Option<T>
where
  G: Fn(&mut Cx, ast::DecOne) -> Option<T>,
  S: FnOnce(&mut Cx, Vec<Option<T>>, SyntaxNodePtr) -> Option<T>,
{
  let dec = dec?;
  let mut decs = Vec::<Option<T>>::new();
  for dwt_in_seq in dec.dec_with_tail_in_seqs() {
    if let Some(semi) = dwt_in_seq.semicolon() {
      cx.err(semi.text_range(), ErrorKind::UnnecessarySemicolon);
    }
    let dwt = match dwt_in_seq.dec_with_tail() {
      Some(x) => x,
      None => continue,
    };
    if let Some(tail) = dwt.sharing_tails().next() {
      cx.err(tail.syntax().text_range(), ErrorKind::InvalidSharingType);
    }
    for dec in dwt.dec_in_seqs() {
      if let Some(semi) = dec.semicolon() {
        cx.err(semi.text_range(), ErrorKind::UnnecessarySemicolon);
      }
      decs.push(g(cx, dec.dec_one()?));
    }
  }
  if decs.len() == 1 {
    decs.pop().unwrap()
  } else {
    s(cx, decs, SyntaxNodePtr::new(dec.syntax()))
  }
}

pub(crate) fn get_top_dec(cx: &mut Cx, dec: Option<ast::Dec>) -> sml_hir::StrDecIdx {
  get_dec_flavor(cx, dec, get_top_dec_one, |cx, decs, ptr| {
    cx.str_dec(sml_hir::StrDec::Seq(decs), ptr)
  })
}

fn get_top_dec_one(cx: &mut Cx, top_dec: ast::DecOne) -> sml_hir::StrDecIdx {
  match top_dec {
    ast::DecOne::ExpDec(top_dec) => {
      let ptr = SyntaxNodePtr::new(top_dec.syntax());
      let dec = sml_hir::Dec::Val(
        Vec::new(),
        vec![sml_hir::ValBind {
          rec: false,
          // the pat should technically be the variable pattern `it`, but allowing that would
          // require threading through the state of "are we actually allowed to re-bind the special
          // `it` variable", which is possible but annoying.
          //
          // I also don't really want to encourage actually using `it` in source files. like, it's
          // technically allowed to do
          //
          // ```sml
          // 2;
          // val x = it + it
          // ```
          //
          // to have `4` be bound to `x`, but that's weird to do _in a source file_. at a REPL it's
          // fine, but millet doesn't check a REPL, it checks source files.
          pat: cx.pat(sml_hir::Pat::Wild, ptr.clone()),
          exp: exp::get(cx, top_dec.exp()),
        }],
      );
      let dec = cx.dec(dec, ptr.clone());
      cx.str_dec(sml_hir::StrDec::Dec(dec), ptr)
    }
    _ => get_str_dec_one(cx, top_dec),
  }
}

fn get_str_dec(cx: &mut Cx, dec: Option<ast::Dec>) -> sml_hir::StrDecIdx {
  get_dec_flavor(cx, dec, get_str_dec_one, |cx, decs, ptr| {
    cx.str_dec(sml_hir::StrDec::Seq(decs), ptr)
  })
}

fn get_str_dec_one(cx: &mut Cx, str_dec: ast::DecOne) -> sml_hir::StrDecIdx {
  let ptr = SyntaxNodePtr::new(str_dec.syntax());
  let res = match str_dec {
    ast::DecOne::LocalDec(str_dec) => sml_hir::StrDec::Local(
      get_str_dec(cx, str_dec.local_dec()),
      get_str_dec(cx, str_dec.in_dec()),
    ),
    ast::DecOne::StructureDec(str_dec) => sml_hir::StrDec::Structure(
      str_dec
        .str_binds()
        .filter_map(|str_bind| {
          let str_exp = str_bind.eq_str_exp().and_then(|x| x.str_exp());
          if str_exp.is_none() {
            cx.err(str_bind.syntax().text_range(), ErrorKind::MissingRhs);
          }
          Some(sml_hir::StrBind {
            name: get_name(str_bind.name())?,
            str_exp: with_ascription_tail(cx, str_exp, str_bind.ascription_tail()),
          })
        })
        .collect(),
    ),
    ast::DecOne::SignatureDec(str_dec) => sml_hir::StrDec::Signature(
      str_dec
        .sig_binds()
        .filter_map(|sig_bind| {
          Some(sml_hir::SigBind {
            name: get_name(sig_bind.name())?,
            sig_exp: get_sig_exp(cx, sig_bind.sig_exp()),
          })
        })
        .collect(),
    ),
    ast::DecOne::FunctorDec(str_dec) => sml_hir::StrDec::Functor(
      str_dec
        .functor_binds()
        .filter_map(|fun_bind| {
          let functor_name = get_name(fun_bind.functor_name())?;
          let body = with_ascription_tail(cx, fun_bind.body(), fun_bind.ascription_tail());
          let (param_name, param_sig, body) = match fun_bind.functor_arg()? {
            ast::FunctorArg::FunctorArgNameSigExp(arg) => {
              (get_name(arg.name())?, get_sig_exp(cx, arg.sig_exp()), body)
            }
            ast::FunctorArg::Spec(arg) => {
              let param_name = cx.fresh();
              let param_sig = sml_hir::SigExp::Spec(get_spec(cx, Some(arg)));
              let param_sig = cx.sig_exp(param_sig, ptr.clone());
              let dec = cx
                .dec(sml_hir::Dec::Open(vec![sml_hir::Path::one(param_name.clone())]), ptr.clone());
              let str_dec = cx.str_dec(sml_hir::StrDec::Dec(dec), ptr.clone());
              let body = cx.str_exp(sml_hir::StrExp::Let(str_dec, body), ptr.clone());
              (param_name, param_sig, body)
            }
          };
          Some(sml_hir::FunctorBind { functor_name, param_name, param_sig, body })
        })
        .collect(),
    ),
    _ => sml_hir::StrDec::Dec(get_one(cx, str_dec)),
  };
  cx.str_dec(res, ptr)
}

fn get_str_exp(cx: &mut Cx, str_exp: Option<ast::StrExp>) -> sml_hir::StrExpIdx {
  let str_exp = str_exp?;
  let ptr = SyntaxNodePtr::new(str_exp.syntax());
  let ret = match str_exp {
    ast::StrExp::StructStrExp(str_exp) => sml_hir::StrExp::Struct(get_str_dec(cx, str_exp.dec())),
    ast::StrExp::PathStrExp(str_exp) => sml_hir::StrExp::Path(get_path(str_exp.path()?)?),
    ast::StrExp::AscriptionStrExp(str_exp) => {
      let (kind, sig_exp) = ascription_tail(cx, str_exp.ascription_tail());
      sml_hir::StrExp::Ascription(get_str_exp(cx, str_exp.str_exp()), kind, sig_exp)
    }
    ast::StrExp::AppStrExp(str_exp) => sml_hir::StrExp::App(
      get_name(str_exp.name())?,
      match str_exp.app_str_exp_arg()? {
        ast::AppStrExpArg::AppStrExpArgStrExp(arg) => get_str_exp(cx, arg.str_exp()),
        ast::AppStrExpArg::Dec(arg) => {
          let sd = get_str_dec(cx, Some(arg));
          cx.str_exp(sml_hir::StrExp::Struct(sd), ptr.clone())
        }
      },
    ),
    ast::StrExp::LetStrExp(str_exp) => {
      sml_hir::StrExp::Let(get_str_dec(cx, str_exp.dec()), get_str_exp(cx, str_exp.str_exp()))
    }
  };
  cx.str_exp(ret, ptr)
}

fn get_sig_exp(cx: &mut Cx, sig_exp: Option<ast::SigExp>) -> sml_hir::SigExpIdx {
  let sig_exp = sig_exp?;
  let ptr = SyntaxNodePtr::new(sig_exp.syntax());
  let ret = match sig_exp {
    ast::SigExp::SigSigExp(sig_exp) => sml_hir::SigExp::Spec(get_spec(cx, sig_exp.spec())),
    ast::SigExp::NameSigExp(sig_exp) => sml_hir::SigExp::Name(get_name(sig_exp.name())?),
    ast::SigExp::WhereTypeSigExp(sig_exp) => sml_hir::SigExp::WhereType(
      get_sig_exp(cx, sig_exp.sig_exp()),
      ty::var_seq(sig_exp.ty_var_seq()),
      get_path(sig_exp.path()?)?,
      ty::get(cx, sig_exp.ty()),
    ),
    ast::SigExp::WhereSigExp(sig_exp) => sml_hir::SigExp::Where(
      get_sig_exp(cx, sig_exp.sig_exp()),
      get_path(sig_exp.lhs()?)?,
      get_path(sig_exp.rhs()?)?,
    ),
  };
  cx.sig_exp(ret, ptr)
}

#[allow(dead_code)]
fn get_spec_of_dec(cx: &mut Cx, dec: Option<ast::Dec>) -> sml_hir::SpecIdx {
  let dec = dec?;
  let mut specs = Vec::<sml_hir::SpecIdx>::new();
  for dwt_in_seq in dec.dec_with_tail_in_seqs() {
    if let Some(semi) = dwt_in_seq.semicolon() {
      cx.err(semi.text_range(), ErrorKind::UnnecessarySemicolon);
    }
    let dwt = match dwt_in_seq.dec_with_tail() {
      Some(x) => x,
      None => continue,
    };
    let ptr = SyntaxNodePtr::new(dwt.syntax());
    let mut inner_specs = Vec::<sml_hir::SpecIdx>::new();
    for dec in dwt.dec_in_seqs() {
      if let Some(semi) = dec.semicolon() {
        cx.err(semi.text_range(), ErrorKind::UnnecessarySemicolon);
      }
      inner_specs.extend(get_spec_one_of_dec(cx, dec.dec_one()?));
    }
    let inner = if inner_specs.len() == 1 {
      inner_specs.pop().unwrap()
    } else {
      cx.spec(sml_hir::Spec::Seq(inner_specs), ptr.clone())
    };
    specs.push(dwt.sharing_tails().fold(inner, |ac, tail| {
      let kind = if tail.type_kw().is_some() {
        sml_hir::SharingKind::Regular
      } else {
        sml_hir::SharingKind::Derived
      };
      let paths_eq: Vec<_> = tail.path_eqs().filter_map(|x| get_path(x.path()?)).collect();
      cx.spec(sml_hir::Spec::Sharing(ac, kind, paths_eq), ptr.clone())
    }));
  }
  if specs.len() == 1 {
    specs.pop().unwrap()
  } else {
    cx.spec(sml_hir::Spec::Seq(specs), SyntaxNodePtr::new(dec.syntax()))
  }
}

fn get_spec(cx: &mut Cx, spec: Option<ast::Spec>) -> sml_hir::SpecIdx {
  let spec = spec?;
  let mut specs: Vec<_> = spec
    .spec_with_tail_in_seqs()
    .map(|x| {
      if let Some(semi) = x.semicolon() {
        cx.err(semi.text_range(), ErrorKind::UnnecessarySemicolon);
      }
      get_spec_with_tail(cx, x.spec_with_tail()?)
    })
    .collect();
  if specs.len() == 1 {
    specs.pop().unwrap()
  } else {
    cx.spec(sml_hir::Spec::Seq(specs), SyntaxNodePtr::new(spec.syntax()))
  }
}

fn get_spec_with_tail(cx: &mut Cx, spec: ast::SpecWithTail) -> sml_hir::SpecIdx {
  let ptr = SyntaxNodePtr::new(spec.syntax());
  let mut specs: Vec<_> = spec
    .spec_in_seqs()
    .map(|x| {
      if let Some(semi) = x.semicolon() {
        cx.err(semi.text_range(), ErrorKind::UnnecessarySemicolon);
      }
      get_spec_one(cx, x.spec_one()?)
    })
    .collect();
  let inner = if specs.len() == 1 {
    specs.pop().unwrap()
  } else {
    cx.spec(sml_hir::Spec::Seq(specs), ptr.clone())
  };
  spec.sharing_tails().fold(inner, |ac, tail| {
    let kind = if tail.type_kw().is_some() {
      sml_hir::SharingKind::Regular
    } else {
      sml_hir::SharingKind::Derived
    };
    let paths_eq: Vec<_> = tail.path_eqs().filter_map(|x| get_path(x.path()?)).collect();
    cx.spec(sml_hir::Spec::Sharing(ac, kind, paths_eq), ptr.clone())
  })
}

/// the Definition doesn't ask us to lower `and` into `seq` but we mostly do anyway, since we have
/// to for `type t = u` specifications.
#[allow(dead_code)]
fn get_spec_one_of_dec(cx: &mut Cx, dec: ast::DecOne) -> Vec<sml_hir::SpecIdx> {
  let ptr = SyntaxNodePtr::new(dec.syntax());
  match dec {
    ast::DecOne::HoleDec(_) => {
      cx.err(dec.syntax().text_range(), ErrorKind::DecHole);
      vec![]
    }
    ast::DecOne::ValDec(dec) => {
      if let Some(tvs) = dec.ty_var_seq() {
        cx.err(tvs.syntax().text_range(), ErrorKind::ValSpecTyVarSeq);
      }
      let descs: Vec<_> = dec
        .val_binds()
        .filter_map(|val_bind| {
          if let Some(x) = val_bind.eq_exp() {
            cx.err(x.syntax().text_range(), ErrorKind::NotSpec);
          }
          if let Some(x) = val_bind.rec_kw() {
            cx.err(x.text_range(), ErrorKind::NotSpec);
          }
          match val_bind.pat()? {
            ast::Pat::TypedPat(ty_pat) => match ty_pat.pat()? {
              ast::Pat::ConPat(con_pat) => {
                if let Some(x) = con_pat.op_kw() {
                  cx.err(x.text_range(), ErrorKind::NotSpec);
                }
                if let Some(x) = con_pat.pat() {
                  cx.err(x.syntax().text_range(), ErrorKind::NotSpec);
                }
                let path = con_pat.path()?;
                let mut iter = path.name_star_eq_dots();
                let fst = iter.next()?;
                if iter.next().is_some() {
                  cx.err(path.syntax().text_range(), ErrorKind::NotSpec);
                }
                let name = str_util::Name::new(fst.name_star_eq()?.token.text());
                let ty = ty::get(cx, ty_pat.ty());
                Some(sml_hir::ValDesc { name, ty })
              }
              pat => {
                cx.err(pat.syntax().text_range(), ErrorKind::NotSpec);
                None
              }
            },
            pat => {
              cx.err(pat.syntax().text_range(), ErrorKind::NotSpec);
              None
            }
          }
        })
        .collect();
      vec![cx.spec(sml_hir::Spec::Val(vec![], descs), ptr)]
    }
    ast::DecOne::TyDec(dec) => {
      let f = match dec.ty_head() {
        None => return vec![],
        Some(head) => match head.kind {
          ast::TyHeadKind::TypeKw => sml_hir::Spec::Ty,
          ast::TyHeadKind::EqtypeKw => sml_hir::Spec::EqTy,
        },
      };
      dec
        .ty_binds()
        .filter_map(|ty_desc| {
          let ty_vars = ty::var_seq(ty_desc.ty_var_seq());
          let name = get_name(ty_desc.name())?;
          let mut ret = f(sml_hir::TyDesc { name: name.clone(), ty_vars: ty_vars.clone() });
          if let Some(ty) = ty_desc.eq_ty() {
            let ty = ty::get(cx, ty.ty());
            let spec_idx = cx.spec(ret, ptr.clone());
            let sig_exp = cx.sig_exp(sml_hir::SigExp::Spec(spec_idx), ptr.clone());
            let sig_exp = cx.sig_exp(
              sml_hir::SigExp::WhereType(sig_exp, ty_vars, sml_hir::Path::one(name), ty),
              ptr.clone(),
            );
            ret = sml_hir::Spec::Include(sig_exp);
          }
          Some(cx.spec(ret, ptr.clone()))
        })
        .collect()
    }
    ast::DecOne::DatDec(dec) => {
      if let Some(with_type) = dec.with_type() {
        cx.err(
          with_type.syntax().text_range(),
          ErrorKind::Unsupported("`withtype` in specifications"),
        );
      }
      // need to collect to end the exclusive borrow on `cx`
      #[allow(clippy::needless_collect)]
      let binds: Vec<_> = dat_binds(cx, dec.dat_binds()).collect();
      binds.into_iter().map(|x| cx.spec(sml_hir::Spec::Datatype(x), ptr.clone())).collect()
    }
    ast::DecOne::DatCopyDec(dec) => get_name(dec.name())
      .zip(dec.path().and_then(get_path))
      .map(|(name, path)| vec![cx.spec(sml_hir::Spec::DatatypeCopy(name, path), ptr)])
      .unwrap_or_default(),
    ast::DecOne::ExDec(dec) => dec
      .ex_binds()
      .filter_map(|ex_bind| {
        let name = str_util::Name::new(ex_bind.name_star_eq()?.token.text());
        let ty = ex_bind.ex_bind_inner().map(|inner| match inner {
          ast::ExBindInner::OfTy(of_ty) => ty::get(cx, of_ty.ty()),
          ast::ExBindInner::EqPath(eq_path) => {
            cx.err(eq_path.syntax().text_range(), ErrorKind::NotSpec);
            None
          }
        });
        Some(cx.spec(sml_hir::Spec::Exception(sml_hir::ExDesc { name, ty }), ptr.clone()))
      })
      .collect(),
    ast::DecOne::StructureDec(dec) => dec
      .str_binds()
      .filter_map(|str_bind| {
        if let Some(x) = str_bind.eq_str_exp() {
          cx.err(x.syntax().text_range(), ErrorKind::NotSpec);
        }
        let name = get_name(str_bind.name())?;
        let sig_exp = match str_bind.ascription_tail() {
          Some(tail) => match tail.ascription() {
            Some(asc) => match asc.kind {
              ast::AscriptionKind::Colon => Ok(get_sig_exp(cx, tail.sig_exp())),
              ast::AscriptionKind::ColonGt => Err(asc.token.text_range()),
            },
            None => Err(tail.syntax().text_range()),
          },
          None => Err(str_bind.syntax().text_range()),
        };
        let sig_exp = match sig_exp {
          Ok(x) => x,
          Err(range) => {
            cx.err(range, ErrorKind::NotSpec);
            None
          }
        };
        let spec = sml_hir::Spec::Str(sml_hir::StrDesc { name, sig_exp });
        Some(cx.spec(spec, ptr.clone()))
      })
      .collect(),
    ast::DecOne::IncludeDec(dec) => {
      let specs: Vec<_> = dec
        .sig_exps()
        .map(|x| {
          let spec = sml_hir::Spec::Include(get_sig_exp(cx, Some(x)));
          cx.spec(spec, ptr.clone())
        })
        .collect();
      if specs.is_empty() {
        cx.err(dec.syntax().text_range(), ErrorKind::RequiresOperand);
      }
      specs
    }
    ast::DecOne::FunDec(_)
    | ast::DecOne::AbstypeDec(_)
    | ast::DecOne::OpenDec(_)
    | ast::DecOne::InfixDec(_)
    | ast::DecOne::InfixrDec(_)
    | ast::DecOne::NonfixDec(_)
    | ast::DecOne::DoDec(_)
    | ast::DecOne::LocalDec(_)
    | ast::DecOne::SignatureDec(_)
    | ast::DecOne::FunctorDec(_)
    | ast::DecOne::ExpDec(_) => {
      cx.err(dec.syntax().text_range(), ErrorKind::NotSpec);
      vec![]
    }
  }
}

fn get_spec_one(cx: &mut Cx, spec: ast::SpecOne) -> sml_hir::SpecIdx {
  let ptr = SyntaxNodePtr::new(spec.syntax());
  let ret = match spec {
    ast::SpecOne::ValSpec(spec) => {
      if let Some(tvs) = spec.ty_var_seq() {
        cx.err(tvs.syntax().text_range(), ErrorKind::ValSpecTyVarSeq);
      }
      sml_hir::Spec::Val(
        Vec::new(),
        spec
          .val_descs()
          .filter_map(|x| {
            Some(sml_hir::ValDesc {
              name: str_util::Name::new(x.name_star_eq()?.token.text()),
              ty: ty::get(cx, x.ty()),
            })
          })
          .collect(),
      )
    }
    ast::SpecOne::TySpec(spec) => ty_descs(cx, ptr.clone(), spec.ty_descs(), sml_hir::Spec::Ty),
    ast::SpecOne::EqTySpec(spec) => ty_descs(cx, ptr.clone(), spec.ty_descs(), sml_hir::Spec::EqTy),
    ast::SpecOne::DatSpec(spec) => {
      if let Some(with_type) = spec.with_type() {
        cx.err(
          with_type.syntax().text_range(),
          ErrorKind::Unsupported("`withtype` in specifications"),
        );
      }
      let specs: Vec<_> = dat_binds(cx, spec.dat_binds()).map(sml_hir::Spec::Datatype).collect();
      seq(cx, ptr.clone(), specs)
    }
    ast::SpecOne::DatCopySpec(spec) => {
      sml_hir::Spec::DatatypeCopy(get_name(spec.name())?, get_path(spec.path()?)?)
    }
    ast::SpecOne::ExSpec(spec) => {
      let specs: Vec<_> = spec
        .ex_descs()
        .filter_map(|x| {
          Some(sml_hir::Spec::Exception(sml_hir::ExDesc {
            name: str_util::Name::new(x.name_star_eq()?.token.text()),
            ty: x.of_ty().map(|x| ty::get(cx, x.ty())),
          }))
        })
        .collect();
      seq(cx, ptr.clone(), specs)
    }
    ast::SpecOne::StrSpec(spec) => {
      let specs: Vec<_> = spec
        .str_descs()
        .filter_map(|x| {
          Some(sml_hir::Spec::Str(sml_hir::StrDesc {
            name: get_name(x.name())?,
            sig_exp: get_sig_exp(cx, x.sig_exp()),
          }))
        })
        .collect();
      seq(cx, ptr.clone(), specs)
    }
    ast::SpecOne::IncludeSpec(spec) => {
      let specs: Vec<_> =
        spec.sig_exps().map(|x| sml_hir::Spec::Include(get_sig_exp(cx, Some(x)))).collect();
      if specs.is_empty() {
        cx.err(spec.syntax().text_range(), ErrorKind::RequiresOperand);
      }
      seq(cx, ptr.clone(), specs)
    }
  };
  cx.spec(ret, ptr)
}

fn seq(cx: &mut Cx, ptr: SyntaxNodePtr, mut specs: Vec<sml_hir::Spec>) -> sml_hir::Spec {
  if specs.len() == 1 {
    specs.pop().unwrap()
  } else {
    sml_hir::Spec::Seq(specs.into_iter().map(|val| cx.spec(val, ptr.clone())).collect())
  }
}

fn ty_descs<I, F>(cx: &mut Cx, ptr: SyntaxNodePtr, iter: I, f: F) -> sml_hir::Spec
where
  I: Iterator<Item = ast::TyDesc>,
  F: Fn(sml_hir::TyDesc) -> sml_hir::Spec,
{
  let specs: Vec<_> = iter
    .filter_map(|ty_desc| {
      let ty_vars = ty::var_seq(ty_desc.ty_var_seq());
      let name = get_name(ty_desc.name())?;
      let mut ret = f(sml_hir::TyDesc { name: name.clone(), ty_vars: ty_vars.clone() });
      if let Some(ty) = ty_desc.eq_ty() {
        let ty = ty::get(cx, ty.ty());
        let spec_idx = cx.spec(ret, ptr.clone());
        let sig_exp = cx.sig_exp(sml_hir::SigExp::Spec(spec_idx), ptr.clone());
        let sig_exp = cx.sig_exp(
          sml_hir::SigExp::WhereType(sig_exp, ty_vars, sml_hir::Path::one(name), ty),
          ptr.clone(),
        );
        ret = sml_hir::Spec::Include(sig_exp);
      }
      Some(ret)
    })
    .collect();
  seq(cx, ptr, specs)
}

fn ascription_tail(
  cx: &mut Cx,
  tail: Option<ast::AscriptionTail>,
) -> (sml_hir::Ascription, sml_hir::SigExpIdx) {
  let kind = tail.as_ref().and_then(|x| x.ascription()).map_or(
    sml_hir::Ascription::Transparent,
    |x| match x.kind {
      ast::AscriptionKind::Colon => sml_hir::Ascription::Transparent,
      ast::AscriptionKind::ColonGt => sml_hir::Ascription::Opaque,
    },
  );
  (kind, get_sig_exp(cx, tail.and_then(|x| x.sig_exp())))
}

fn with_ascription_tail(
  cx: &mut Cx,
  str_exp: Option<ast::StrExp>,
  tail: Option<ast::AscriptionTail>,
) -> sml_hir::StrExpIdx {
  let str_exp = str_exp?;
  let ptr = SyntaxNodePtr::new(str_exp.syntax());
  let mut ret = get_str_exp(cx, Some(str_exp));
  if let Some(tail) = tail {
    let (kind, sig_exp) = ascription_tail(cx, Some(tail));
    let asc = sml_hir::StrExp::Ascription(ret, kind, sig_exp);
    ret = cx.str_exp(asc, ptr);
  }
  ret
}

pub(crate) fn get(cx: &mut Cx, dec: Option<ast::Dec>) -> sml_hir::DecIdx {
  get_dec_flavor(cx, dec, get_one, |cx, decs, ptr| cx.dec(sml_hir::Dec::Seq(decs), ptr))
}

fn get_one(cx: &mut Cx, dec: ast::DecOne) -> sml_hir::DecIdx {
  let ptr = SyntaxNodePtr::new(dec.syntax());
  let ret = match dec {
    ast::DecOne::HoleDec(_) => {
      cx.err(dec.syntax().text_range(), ErrorKind::DecHole);
      return None;
    }
    ast::DecOne::ValDec(dec) => sml_hir::Dec::Val(
      ty::var_seq(dec.ty_var_seq()),
      dec
        .val_binds()
        .map(|val_bind| {
          let exp = val_bind.eq_exp().and_then(|x| x.exp());
          if exp.is_none() {
            cx.err(val_bind.syntax().text_range(), ErrorKind::MissingRhs);
          }
          sml_hir::ValBind {
            rec: val_bind.rec_kw().is_some(),
            pat: pat::get(cx, val_bind.pat()),
            exp: exp::get(cx, exp),
          }
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
          let ptr = SyntaxNodePtr::new(fun_bind.syntax());
          let mut name = None::<sml_syntax::SyntaxToken>;
          let mut num_pats = None::<usize>;
          let arms: Vec<_> = fun_bind
            .fun_bind_cases()
            .map(|case| {
              let mut pats = Vec::<sml_hir::PatIdx>::with_capacity(1);
              let head_name = case
                .fun_bind_case_head()
                .and_then(|head| match head {
                  ast::FunBindCaseHead::PrefixFunBindCaseHead(head) => head.name_star_eq(),
                  ast::FunBindCaseHead::InfixFunBindCaseHead(head) => {
                    let lhs = head.lhs();
                    let rhs = head.rhs();
                    let tup = tuple([pat::get(cx, lhs), pat::get(cx, rhs)]);
                    pats.push(cx.pat(tup, ptr.clone()));
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
              pats.extend(case.pats().map(|pat| pat::get(cx, Some(pat))));
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
              let pat = if pats.len() == 1 {
                pats.pop().unwrap()
              } else {
                cx.pat(pat::tuple(pats), ptr.clone())
              };
              let ty = case.ty_annotation().map(|x| ty::get(cx, x.ty()));
              let exp = case.exp().and_then(|exp| {
                let ptr = SyntaxNodePtr::new(exp.syntax());
                let mut exp = exp::get(cx, Some(exp));
                if let Some(ty) = ty {
                  exp = cx.exp(sml_hir::Exp::Typed(exp, ty), ptr);
                }
                exp
              });
              (pat, exp)
            })
            .collect();
          // not the greatest, since we have no body at all if the ptrs are None. but if they were
          // both None, then something's very strange about the fun_bind_cases anyway.
          let exp = {
            let arg_names: Vec<_> = (0..num_pats.unwrap_or(1)).map(|_| cx.fresh()).collect();
            let mut arg_exprs =
              arg_names.iter().map(|name| cx.exp(exp::name(name.as_str()), ptr.clone()));
            let head = if arg_exprs.len() == 1 {
              arg_exprs.next().unwrap()
            } else {
              let tup = exp::tuple(arg_exprs);
              cx.exp(tup, ptr.clone())
            };
            let case = exp::case(cx, head, arms, ptr.clone());
            arg_names.into_iter().rev().fold(cx.exp(case, ptr.clone()), |body, name| {
              let pat = cx.pat(pat::name(name.as_str()), ptr.clone());
              cx.exp(sml_hir::Exp::Fn(vec![(pat, body)]), ptr.clone())
            })
          };
          sml_hir::ValBind {
            rec: true,
            pat: name.and_then(|name| cx.pat(pat::name(name.text()), ptr)),
            exp,
          }
        })
        .collect();
      sml_hir::Dec::Val(ty_vars, val_binds)
    }
    ast::DecOne::TyDec(dec) => {
      let hd = dec.ty_head()?;
      match hd.kind {
        ast::TyHeadKind::TypeKw => {}
        ast::TyHeadKind::EqtypeKw => cx.err(hd.token.text_range(), ErrorKind::InvalidEqtype),
      }
      sml_hir::Dec::Ty(ty_binds(cx, dec.ty_binds()))
    }
    ast::DecOne::DatDec(dec) => {
      let dbs: Vec<_> = dat_binds(cx, dec.dat_binds()).collect();
      let tbs = ty_binds(cx, dec.with_type().into_iter().flat_map(|x| x.ty_binds()));
      sml_hir::Dec::Datatype(dbs, tbs)
    }
    ast::DecOne::DatCopyDec(dec) => {
      sml_hir::Dec::DatatypeCopy(get_name(dec.name())?, get_path(dec.path()?)?)
    }
    ast::DecOne::AbstypeDec(dec) => {
      let dbs: Vec<_> = dat_binds(cx, dec.dat_binds()).collect();
      let tbs = ty_binds(cx, dec.with_type().into_iter().flat_map(|x| x.ty_binds()));
      let inner = get(cx, dec.dec());
      sml_hir::Dec::Abstype(dbs, tbs, inner)
    }
    ast::DecOne::ExDec(dec) => sml_hir::Dec::Exception(
      dec
        .ex_binds()
        .filter_map(|ex_bind| {
          let name = str_util::Name::new(ex_bind.name_star_eq()?.token.text());
          let ret = match ex_bind.ex_bind_inner() {
            None => sml_hir::ExBind::New(name, None),
            Some(ast::ExBindInner::OfTy(x)) => {
              sml_hir::ExBind::New(name, Some(ty::get(cx, x.ty())))
            }
            Some(ast::ExBindInner::EqPath(x)) => sml_hir::ExBind::Copy(name, get_path(x.path()?)?),
          };
          Some(ret)
        })
        .collect(),
    ),
    ast::DecOne::LocalDec(dec) => {
      sml_hir::Dec::Local(get(cx, dec.local_dec()), get(cx, dec.in_dec()))
    }
    ast::DecOne::OpenDec(dec) => {
      let paths: Vec<_> = dec.paths().filter_map(get_path).collect();
      if paths.is_empty() {
        cx.err(dec.syntax().text_range(), ErrorKind::RequiresOperand);
      }
      sml_hir::Dec::Open(paths)
    }
    ast::DecOne::InfixDec(_) | ast::DecOne::InfixrDec(_) | ast::DecOne::NonfixDec(_) => {
      return None
    }
    ast::DecOne::DoDec(ref inner) => {
      // emit an error, but lower anyway.
      cx.err(dec.syntax().text_range(), ErrorKind::Unsupported("`do` declarations"));
      sml_hir::Dec::Val(
        Vec::new(),
        vec![sml_hir::ValBind {
          rec: false,
          pat: cx.pat(pat::tuple([]), ptr.clone()),
          exp: exp::get(cx, inner.exp()),
        }],
      )
    }
    ast::DecOne::StructureDec(_)
    | ast::DecOne::SignatureDec(_)
    | ast::DecOne::FunctorDec(_)
    | ast::DecOne::IncludeDec(_) => {
      cx.err(dec.syntax().text_range(), ErrorKind::DecNotAllowedHere);
      return None;
    }
    ast::DecOne::ExpDec(_) => {
      cx.err(dec.syntax().text_range(), ErrorKind::ExpNotAllowedHere);
      return None;
    }
  };
  cx.dec(ret, ptr)
}

fn dat_binds<'a, I>(cx: &'a mut Cx, iter: I) -> impl Iterator<Item = sml_hir::DatBind> + 'a
where
  I: Iterator<Item = ast::DatBind> + 'a,
{
  iter.filter_map(|dat_bind| {
    if let Some(bar) = dat_bind.bar() {
      cx.err(bar.text_range(), ErrorKind::PrecedingBar);
    }
    Some(sml_hir::DatBind {
      ty_vars: ty::var_seq(dat_bind.ty_var_seq()),
      name: get_name(dat_bind.name())?,
      cons: dat_bind
        .con_binds()
        .filter_map(|con_bind| {
          Some(sml_hir::ConBind {
            name: str_util::Name::new(con_bind.name_star_eq()?.token.text()),
            ty: con_bind.of_ty().map(|x| ty::get(cx, x.ty())),
          })
        })
        .collect(),
    })
  })
}

fn ty_binds<I>(cx: &mut Cx, iter: I) -> Vec<sml_hir::TyBind>
where
  I: Iterator<Item = ast::TyBind>,
{
  iter
    .filter_map(|ty_bind| {
      let name = get_name(ty_bind.name())?;
      let ty = ty_bind.eq_ty().and_then(|x| x.ty());
      if ty.is_none() {
        cx.err(ty_bind.syntax().text_range(), ErrorKind::MissingRhs);
      }
      Some(sml_hir::TyBind {
        ty_vars: ty::var_seq(ty_bind.ty_var_seq()),
        name,
        ty: ty::get(cx, ty),
      })
    })
    .collect()
}
