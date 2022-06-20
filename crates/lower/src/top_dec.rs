use crate::common::{get_name, get_path};
use crate::util::{Cx, ErrorKind};
use crate::{dec, ty};
use syntax::ast::{self, AstNode as _, AstPtr};

pub(crate) fn get(cx: &mut Cx, top_dec: ast::StrDecOne) -> hir::TopDecIdx {
  let ptr = AstPtr::new(&top_dec);
  let ret = match top_dec {
    ast::StrDecOne::DecStrDec(_)
    | ast::StrDecOne::StructureStrDec(_)
    | ast::StrDecOne::LocalStrDec(_) => hir::TopDec::Str(get_str_dec_one(cx, top_dec)),
    ast::StrDecOne::SigDec(top_dec) => hir::TopDec::Sig(
      top_dec
        .sig_binds()
        .filter_map(|sig_bind| {
          Some(hir::SigBind {
            name: get_name(sig_bind.name())?,
            sig_exp: get_sig_exp(cx, sig_bind.sig_exp()),
          })
        })
        .collect(),
    ),
    ast::StrDecOne::FunctorDec(top_dec) => hir::TopDec::Functor(
      top_dec
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
              let param_sig = hir::SigExp::Spec(get_spec(cx, Some(arg)));
              let param_sig = cx.sig_exp_in_top_dec(param_sig, ptr.clone());
              let dec = cx.dec_in_top_dec(
                hir::Dec::Open(vec![hir::Path::one(param_name.clone())]),
                ptr.clone(),
              );
              let str_dec = cx.str_dec_in_top_dec(hir::StrDec::Dec(dec), ptr.clone());
              let body = cx.str_exp_in_top_dec(hir::StrExp::Let(str_dec, body), ptr.clone());
              (param_name, param_sig, body)
            }
          };
          Some(hir::FunctorBind {
            functor_name,
            param_name,
            param_sig,
            body,
          })
        })
        .collect(),
    ),
  };
  cx.top_dec(ret, ptr)
}

fn get_str_dec(cx: &mut Cx, str_dec: Option<ast::StrDec>) -> hir::StrDecIdx {
  let str_dec = str_dec?;
  let mut str_decs: Vec<_> = str_dec
    .str_dec_in_seqs()
    .map(|x| get_str_dec_one(cx, x.str_dec_one()?))
    .collect();
  if str_decs.len() == 1 {
    str_decs.pop().unwrap()
  } else {
    cx.str_dec_seq(str_decs, AstPtr::new(&str_dec))
  }
}

fn get_str_dec_one(cx: &mut Cx, str_dec: ast::StrDecOne) -> hir::StrDecIdx {
  let ptr = AstPtr::new(&str_dec);
  let res = match str_dec {
    ast::StrDecOne::DecStrDec(str_dec) => hir::StrDec::Dec(dec::get_one(cx, str_dec.dec_one()?)),
    ast::StrDecOne::StructureStrDec(str_dec) => hir::StrDec::Structure(
      str_dec
        .str_binds()
        .filter_map(|str_bind| {
          Some(hir::StrBind {
            name: get_name(str_bind.name())?,
            str_exp: with_ascription_tail(cx, str_bind.str_exp(), str_bind.ascription_tail()),
          })
        })
        .collect(),
    ),
    ast::StrDecOne::LocalStrDec(str_dec) => hir::StrDec::Local(
      get_str_dec(cx, str_dec.local_dec()),
      get_str_dec(cx, str_dec.in_dec()),
    ),
    ast::StrDecOne::SigDec(dec) => {
      cx.err(dec.syntax().text_range(), ErrorKind::MustBeTopLevel);
      return None;
    }
    ast::StrDecOne::FunctorDec(dec) => {
      cx.err(dec.syntax().text_range(), ErrorKind::MustBeTopLevel);
      return None;
    }
  };
  cx.str_dec_one(res, ptr)
}

fn get_str_exp(cx: &mut Cx, str_exp: Option<ast::StrExp>) -> hir::StrExpIdx {
  let str_exp = str_exp?;
  let ptr = AstPtr::new(&str_exp);
  let ret = match str_exp {
    ast::StrExp::StructStrExp(str_exp) => hir::StrExp::Struct(get_str_dec(cx, str_exp.str_dec())),
    ast::StrExp::PathStrExp(str_exp) => hir::StrExp::Path(get_path(str_exp.path()?)?),
    ast::StrExp::AscriptionStrExp(str_exp) => {
      let (kind, sig_exp) = ascription_tail(cx, str_exp.ascription_tail());
      hir::StrExp::Ascription(get_str_exp(cx, str_exp.str_exp()), kind, sig_exp)
    }
    ast::StrExp::AppStrExp(str_exp) => hir::StrExp::App(
      get_name(str_exp.name())?,
      match str_exp.app_str_exp_arg()? {
        ast::AppStrExpArg::AppStrExpArgStrExp(arg) => get_str_exp(cx, arg.str_exp()),
        ast::AppStrExpArg::StrDec(arg) => {
          let sd = get_str_dec(cx, Some(arg));
          cx.str_exp(hir::StrExp::Struct(sd), ptr.clone())
        }
      },
    ),
    ast::StrExp::LetStrExp(str_exp) => hir::StrExp::Let(
      get_str_dec(cx, str_exp.str_dec()),
      get_str_exp(cx, str_exp.str_exp()),
    ),
  };
  cx.str_exp(ret, ptr)
}

fn get_sig_exp(cx: &mut Cx, sig_exp: Option<ast::SigExp>) -> hir::SigExpIdx {
  let sig_exp = sig_exp?;
  let ptr = AstPtr::new(&sig_exp);
  let ret = match sig_exp {
    ast::SigExp::SigSigExp(sig_exp) => hir::SigExp::Spec(get_spec(cx, sig_exp.spec())),
    ast::SigExp::NameSigExp(sig_exp) => hir::SigExp::Name(get_name(sig_exp.name())?),
    ast::SigExp::WhereSigExp(sig_exp) => hir::SigExp::Where(
      get_sig_exp(cx, sig_exp.sig_exp()),
      ty::var_seq(sig_exp.ty_var_seq()),
      get_path(sig_exp.path()?)?,
      ty::get(cx, sig_exp.ty()),
    ),
  };
  cx.sig_exp(ret, ptr)
}

fn get_spec(cx: &mut Cx, spec: Option<ast::Spec>) -> hir::SpecIdx {
  let spec = spec?;
  let mut specs: Vec<_> = spec
    .spec_with_tail_in_seqs()
    .map(|x| get_spec_with_tail(cx, x.spec_with_tail()?))
    .collect();
  if specs.len() == 1 {
    specs.pop().unwrap()
  } else {
    cx.spec(hir::Spec::Seq(specs), AstPtr::new(&spec))
  }
}

fn get_spec_with_tail(cx: &mut Cx, spec: ast::SpecWithTail) -> hir::SpecIdx {
  let ptr = AstPtr::new(&spec);
  let mut specs: Vec<_> = spec
    .spec_in_seqs()
    .map(|x| get_spec_one(cx, x.spec_one()?))
    .collect();
  let inner = if specs.len() == 1 {
    specs.pop().unwrap()
  } else {
    cx.spec_with_tail(hir::Spec::Seq(specs), ptr.clone())
  };
  spec.sharing_tails().fold(inner, |ac, tail| {
    let kind = if tail.type_kw().is_some() {
      hir::SharingKind::Regular
    } else {
      hir::SharingKind::Derived
    };
    let paths_eq: Vec<_> = tail
      .path_eqs()
      .filter_map(|x| get_path(x.path()?))
      .collect();
    cx.spec_with_tail(hir::Spec::Sharing(ac, kind, paths_eq), ptr.clone())
  })
}

/// the Definition doesn't ask us to lower `and` into `seq` but we mostly do anyway, since we have
/// to for `type t = u` specifications.
fn get_spec_one(cx: &mut Cx, spec: ast::SpecOne) -> hir::SpecIdx {
  let ptr = AstPtr::new(&spec);
  let ret = match spec {
    ast::SpecOne::ValSpec(spec) => hir::Spec::Val(
      Vec::new(),
      spec
        .val_descs()
        .filter_map(|x| {
          Some(hir::ValDesc {
            name: hir::Name::new(x.name_star_eq()?.token.text()),
            ty: ty::get(cx, x.ty()),
          })
        })
        .collect(),
    ),
    ast::SpecOne::TySpec(spec) => ty_descs(cx, ptr.clone(), spec.ty_descs(), hir::Spec::Ty),
    ast::SpecOne::EqTySpec(spec) => ty_descs(cx, ptr.clone(), spec.ty_descs(), hir::Spec::EqTy),
    ast::SpecOne::DatSpec(spec) => {
      let specs: Vec<_> = dec::dat_binds(cx, spec.dat_binds())
        .map(hir::Spec::Datatype)
        .collect();
      seq(cx, ptr.clone(), specs)
    }
    ast::SpecOne::DatCopySpec(spec) => {
      hir::Spec::DatatypeCopy(get_name(spec.name())?, get_path(spec.path()?)?)
    }
    ast::SpecOne::ExSpec(spec) => {
      let specs: Vec<_> = spec
        .ex_descs()
        .filter_map(|x| {
          Some(hir::Spec::Exception(hir::ExDesc {
            name: hir::Name::new(x.name_star_eq()?.token.text()),
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
          Some(hir::Spec::Str(hir::StrDesc {
            name: get_name(x.name())?,
            sig_exp: get_sig_exp(cx, x.sig_exp()),
          }))
        })
        .collect();
      seq(cx, ptr.clone(), specs)
    }
    ast::SpecOne::IncludeSpec(spec) => {
      let specs: Vec<_> = spec
        .sig_exps()
        .map(|x| hir::Spec::Include(get_sig_exp(cx, Some(x))))
        .collect();
      seq(cx, ptr.clone(), specs)
    }
  };
  cx.spec_one(ret, ptr)
}

fn seq(cx: &mut Cx, ptr: AstPtr<ast::SpecOne>, mut specs: Vec<hir::Spec>) -> hir::Spec {
  if specs.len() == 1 {
    specs.pop().unwrap()
  } else {
    hir::Spec::Seq(
      specs
        .into_iter()
        .map(|val| cx.spec_one(val, ptr.clone()))
        .collect(),
    )
  }
}

fn ty_descs<I, F>(cx: &mut Cx, ptr: AstPtr<ast::SpecOne>, iter: I, f: F) -> hir::Spec
where
  I: Iterator<Item = ast::TyDesc>,
  F: Fn(hir::TyDesc) -> hir::Spec,
{
  let specs: Vec<_> = iter
    .filter_map(|ty_desc| {
      let ty_vars = ty::var_seq(ty_desc.ty_var_seq());
      let name = get_name(ty_desc.name())?;
      let mut ret = f(hir::TyDesc {
        name: name.clone(),
        ty_vars: ty_vars.clone(),
      });
      if let Some(ty) = ty_desc.eq_ty() {
        let ty = ty::get(cx, ty.ty());
        let spec_idx = cx.spec_one(ret, ptr.clone());
        let sig_exp = cx.sig_exp_in_spec_one(hir::SigExp::Spec(spec_idx), ptr.clone());
        let sig_exp = cx.sig_exp_in_spec_one(
          hir::SigExp::Where(sig_exp, ty_vars, hir::Path::one(name), ty),
          ptr.clone(),
        );
        ret = hir::Spec::Include(sig_exp);
      }
      Some(ret)
    })
    .collect();
  seq(cx, ptr, specs)
}

fn ascription_tail(
  cx: &mut Cx,
  tail: Option<ast::AscriptionTail>,
) -> (hir::Ascription, hir::SigExpIdx) {
  let kind = tail
    .as_ref()
    .and_then(|x| x.ascription())
    .map_or(hir::Ascription::Transparent, |x| match x.kind {
      ast::AscriptionKind::Colon => hir::Ascription::Transparent,
      ast::AscriptionKind::ColonGt => hir::Ascription::Opaque,
    });
  (kind, get_sig_exp(cx, tail.and_then(|x| x.sig_exp())))
}

fn with_ascription_tail(
  cx: &mut Cx,
  str_exp: Option<ast::StrExp>,
  tail: Option<ast::AscriptionTail>,
) -> hir::StrExpIdx {
  let str_exp = str_exp?;
  let ptr = AstPtr::new(&str_exp);
  let mut ret = get_str_exp(cx, Some(str_exp));
  if let Some(tail) = tail {
    let (kind, sig_exp) = ascription_tail(cx, Some(tail));
    let asc = hir::StrExp::Ascription(ret, kind, sig_exp);
    ret = cx.str_exp(asc, ptr);
  }
  ret
}
