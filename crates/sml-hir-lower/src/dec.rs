//! Lowering declarations.
//!
//! This is where AST declarations become various other things. They can be lowered into HIR
//! declarations or other HIR things.

use crate::common::{forbid_opaque_asc, get_name, get_path};
use crate::pat::{self, tuple};
use crate::util::{Disallowed, ErrorKind, MissingRhs, St};
use crate::{exp, ty};
use sml_syntax::ast::{self, AstNode as _, SyntaxNodePtr};

fn get_dec_flavor<T, F, I>(st: &mut St<'_>, iter: I, f: F) -> Vec<T>
where
  F: Fn(&mut St<'_>, ast::DecOne) -> Option<T>,
  I: Iterator<Item = ast::Dec>,
{
  let mut ret = Vec::<T>::new();
  for dec in iter {
    if let Some(semi) = dec.semicolon() {
      st.err_tok(&semi, ErrorKind::UnnecessarySemicolon);
    }
    let Some(dwt) = dec.dec_with_tail() else { continue };
    if let Some(tail) = dwt.sharing_tails().next() {
      st.err(tail.syntax(), ErrorKind::InvalidSharingType);
    }
    for dec in dwt.dec_in_seqs() {
      if let Some(semi) = dec.semicolon() {
        st.err_tok(&semi, ErrorKind::UnnecessarySemicolon);
      }
      ret.extend(dec.dec_one().and_then(|dec_one| f(st, dec_one)));
    }
  }
  ret
}

pub(crate) fn get_top_dec(st: &mut St<'_>, root: &ast::Root) -> sml_hir::StrDecSeq {
  get_dec_flavor(st, root.decs(), |a, b| Some(get_top_dec_one(a, b)))
}

fn get_top_dec_one(st: &mut St<'_>, top_dec: ast::DecOne) -> sml_hir::StrDecIdx {
  let ret = match top_dec {
    ast::DecOne::ExpDec(top_dec) => {
      let ptr = SyntaxNodePtr::new(top_dec.syntax());
      if !st.lang().dec.exp {
        st.err(top_dec.syntax(), ErrorKind::Disallowed(Disallowed::Dec("expression")));
      }
      if st.file_kind().is_sig_or_fun() {
        st.err(top_dec.syntax(), ErrorKind::DiscouragedTopDecKindForSigFun);
      }
      let bind = sml_hir::ValBind {
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
        pat: st.pat(sml_hir::Pat::Wild, ptr),
        exp: exp::get(st, top_dec.exp()),
      };
      let dec = sml_hir::Dec::Val(Vec::new(), vec![bind], sml_hir::ValFlavor::TopLevelExp);
      let dec = st.dec(dec, ptr);
      st.str_dec(sml_hir::StrDec::Dec(vec![dec]), ptr)
    }
    _ => get_str_dec_one(st, top_dec),
  };
  st.mark_has_seen_top_level();
  ret
}

fn get_str_dec<I>(st: &mut St<'_>, iter: I) -> sml_hir::StrDecSeq
where
  I: Iterator<Item = ast::Dec>,
{
  get_dec_flavor(st, iter, |a, b| Some(get_str_dec_one(a, b)))
}

fn get_str_dec_one(st: &mut St<'_>, str_dec: ast::DecOne) -> sml_hir::StrDecIdx {
  let ptr = SyntaxNodePtr::new(str_dec.syntax());
  let res = match str_dec {
    ast::DecOne::StructureDec(str_dec) => {
      if !st.lang().dec.structure {
        st.err(str_dec.syntax(), ErrorKind::Disallowed(Disallowed::Dec("`structure`")));
      }
      if st.is_top_level() && st.file_kind().is_sig_or_fun() {
        st.err(str_dec.syntax(), ErrorKind::DiscouragedTopDecKindForSigFun);
      }
      st.inc_level();
      let iter = str_dec.str_binds().filter_map(|str_bind| {
        let str_exp = str_bind.eq_str_exp().and_then(|x| x.str_exp());
        if str_exp.is_none() {
          st.err(str_bind.syntax(), ErrorKind::MissingRhs(MissingRhs::Dec));
        }
        Some(sml_hir::StrBind {
          name: get_name(str_bind.name())?,
          str_exp: with_ascription_tail(st, str_exp, str_bind.ascription_tail()),
        })
      });
      let binds: Vec<_> = iter.collect();
      st.dec_level();
      sml_hir::StrDec::Structure(binds)
    }
    ast::DecOne::SignatureDec(str_dec) => {
      if !st.lang().dec.signature {
        st.err(str_dec.syntax(), ErrorKind::Disallowed(Disallowed::Dec("`signature`")));
      }
      if let (true, (true, sml_file::Kind::Sig) | (_, sml_file::Kind::Fun)) =
        (st.is_top_level(), (st.seen_top_level(), st.file_kind()))
      {
        st.err(str_dec.syntax(), ErrorKind::DiscouragedTopDecKindForSigFun);
      }
      let iter = str_dec.sig_binds().filter_map(|sig_bind| {
        Some(sml_hir::SigBind {
          name: get_name(sig_bind.name())?,
          sig_exp: get_sig_exp(st, sig_bind.sig_exp()),
        })
      });
      sml_hir::StrDec::Signature(iter.collect())
    }
    ast::DecOne::FunctorDec(str_dec) => {
      if !st.lang().dec.functor {
        st.err(str_dec.syntax(), ErrorKind::Disallowed(Disallowed::Dec("`functor`")));
      }
      if let (true, (true, sml_file::Kind::Fun) | (_, sml_file::Kind::Sig)) =
        (st.is_top_level(), (st.seen_top_level(), st.file_kind()))
      {
        st.err(str_dec.syntax(), ErrorKind::DiscouragedTopDecKindForSigFun);
      }
      st.inc_level();
      let iter = str_dec.functor_binds().filter_map(|fun_bind| {
        let functor_name = get_name(fun_bind.functor_name())?;
        let body = with_ascription_tail(st, fun_bind.body(), fun_bind.ascription_tail());
        let mut iter = fun_bind.functor_args().peekable();
        let (param_name, param_sig, body, flavor) = match iter.peek() {
          Some(ast::FunctorArg::FunctorArgNameSigExp(arg)) => {
            forbid_opaque_asc(st, arg.ascription());
            (get_name(arg.name())?, get_sig_exp(st, arg.sig_exp()), body, sml_hir::Flavor::Plain)
          }
          None | Some(ast::FunctorArg::Dec(_)) => {
            let decs = iter.filter_map(|x| match x {
              ast::FunctorArg::Dec(x) => Some(x),
              ast::FunctorArg::FunctorArgNameSigExp(_) => None,
            });
            let param_name = st.fresh();
            let param_sig = sml_hir::SigExp::Spec(get_spec(st, decs));
            let param_sig = st.sig_exp(param_sig, ptr);
            let dec =
              st.dec(sml_hir::Dec::Open(vec![sml_path::Path::one(param_name.clone())]), ptr);
            let str_dec = st.str_dec(sml_hir::StrDec::Dec(vec![dec]), ptr);
            let body = st.str_exp(sml_hir::StrExp::Let(vec![str_dec], body), ptr);
            (param_name, param_sig, body, sml_hir::Flavor::Sugared)
          }
        };
        Some(sml_hir::FunctorBind { functor_name, param_name, param_sig, body, flavor })
      });
      let binds: Vec<_> = iter.collect();
      st.dec_level();
      sml_hir::StrDec::Functor(binds)
    }
    ast::DecOne::LocalDec(str_dec) => {
      if !st.lang().dec.local {
        st.err(str_dec.syntax(), ErrorKind::Disallowed(Disallowed::Dec("`local`")));
      }
      if st.is_top_level() && st.file_kind().is_sig_or_fun() {
        st.err(str_dec.syntax(), ErrorKind::DiscouragedTopDecKindForSigFun);
      }
      st.inc_level();
      let fst = get_str_dec(st, str_dec.local_dec_hd().into_iter().flat_map(|x| x.decs()));
      st.dec_level();
      let snd = get_str_dec(st, str_dec.local_dec_tl().into_iter().flat_map(|x| x.decs()));
      if fst.is_empty() {
        st.err(str_dec.syntax(), ErrorKind::EmptyLocal);
      }
      sml_hir::StrDec::Local(fst, snd)
    }
    _ => sml_hir::StrDec::Dec(get_one(st, str_dec).into_iter().collect()),
  };
  st.str_dec(res, ptr)
}

fn get_str_exp(st: &mut St<'_>, str_exp: Option<ast::StrExp>) -> sml_hir::StrExpIdx {
  let str_exp = str_exp?;
  let ptr = SyntaxNodePtr::new(str_exp.syntax());
  let ret = match str_exp {
    ast::StrExp::StructStrExp(str_exp) => sml_hir::StrExp::Struct(get_str_dec(st, str_exp.decs())),
    ast::StrExp::PathStrExp(str_exp) => sml_hir::StrExp::Path(get_path(str_exp.path()?)?),
    ast::StrExp::AscriptionStrExp(str_exp) => {
      let (kind, sig_exp) = ascription_tail(st, str_exp.ascription_tail());
      sml_hir::StrExp::Ascription(get_str_exp(st, str_exp.str_exp()), kind, sig_exp)
    }
    ast::StrExp::AppStrExp(str_exp) => {
      let mut iter = str_exp.app_str_exp_args().peekable();
      let (arg, flavor) = match iter.peek() {
        Some(ast::AppStrExpArg::AppStrExpArgStrExp(arg)) => {
          (get_str_exp(st, arg.str_exp()), sml_hir::Flavor::Plain)
        }
        None | Some(ast::AppStrExpArg::Dec(_)) => {
          let decs = iter.filter_map(|x| match x {
            ast::AppStrExpArg::AppStrExpArgStrExp(_) => None,
            ast::AppStrExpArg::Dec(x) => Some(x),
          });
          let sd = get_str_dec(st, decs);
          (st.str_exp(sml_hir::StrExp::Struct(sd), ptr), sml_hir::Flavor::Sugared)
        }
      };
      sml_hir::StrExp::App(get_name(str_exp.name())?, arg, flavor)
    }
    ast::StrExp::LetStrExp(str_exp) => {
      let fst = get_str_dec(st, str_exp.decs());
      let snd = get_str_exp(st, str_exp.str_exp());
      if fst.is_empty() {
        st.err(str_exp.syntax(), ErrorKind::EmptyLet);
      }
      sml_hir::StrExp::Let(fst, snd)
    }
  };
  st.str_exp(ret, ptr)
}

fn get_sig_exp(st: &mut St<'_>, sig_exp: Option<ast::SigExp>) -> sml_hir::SigExpIdx {
  let sig_exp = sig_exp?;
  let ptr = SyntaxNodePtr::new(sig_exp.syntax());
  let ret = match sig_exp {
    ast::SigExp::SigSigExp(sig_exp) => sml_hir::SigExp::Spec(get_spec(st, sig_exp.decs())),
    ast::SigExp::NameSigExp(sig_exp) => sml_hir::SigExp::Name(get_name(sig_exp.name())?),
    ast::SigExp::WhereTypeSigExp(sig_exp) => sml_hir::SigExp::Where(
      get_sig_exp(st, sig_exp.sig_exp()),
      sml_hir::WhereKind::Type(
        ty::var_seq(st, sig_exp.ty_var_seq()),
        get_path(sig_exp.path()?)?,
        ty::get(st, sig_exp.ty()),
      ),
    ),
    ast::SigExp::WhereSigExp(sig_exp) => sml_hir::SigExp::Where(
      get_sig_exp(st, sig_exp.sig_exp()),
      sml_hir::WhereKind::Structure(get_path(sig_exp.lhs()?)?, get_path(sig_exp.rhs()?)?),
    ),
  };
  st.sig_exp(ret, ptr)
}

fn get_spec<I>(st: &mut St<'_>, iter: I) -> sml_hir::SpecSeq
where
  I: Iterator<Item = ast::Dec>,
{
  let mut specs = Vec::<sml_hir::SpecIdx>::new();
  for dec in iter {
    if let Some(semi) = dec.semicolon() {
      st.err_tok(&semi, ErrorKind::UnnecessarySemicolon);
    }
    let Some(dwt) = dec.dec_with_tail() else { continue };
    let ptr = SyntaxNodePtr::new(dwt.syntax());
    let mut inner_specs = Vec::<sml_hir::SpecIdx>::new();
    for dec in dwt.dec_in_seqs() {
      if let Some(semi) = dec.semicolon() {
        st.err_tok(&semi, ErrorKind::UnnecessarySemicolon);
      }
      inner_specs.extend(get_spec_one(st, dec.dec_one()));
    }
    specs.extend(dwt.sharing_tails().fold(inner_specs, |ac, tail| {
      let kind = if tail.type_kw().is_some() {
        sml_hir::SharingKind::Regular
      } else {
        sml_hir::SharingKind::Derived
      };
      let paths_eq: Vec<_> = tail.path_eqs().filter_map(|x| get_path(x.path()?)).collect();
      vec![st.spec(sml_hir::Spec::Sharing(ac, kind, paths_eq), ptr)]
    }));
  }
  specs
}

/// the Definition doesn't ask us to lower `and` into `seq` but we mostly do anyway, since we have
/// to for `type t = u` specifications.
fn get_spec_one(st: &mut St<'_>, dec: Option<ast::DecOne>) -> Vec<sml_hir::SpecIdx> {
  let Some(dec) = dec else { return vec![] };
  let ptr = SyntaxNodePtr::new(dec.syntax());
  match dec {
    ast::DecOne::HoleDec(_) => {
      st.err(dec.syntax(), ErrorKind::DecHole);
      vec![]
    }
    ast::DecOne::ValDec(dec) => {
      if !st.lang().dec.val {
        st.err(dec.syntax(), ErrorKind::Disallowed(Disallowed::Dec("`val`")));
      }
      if let Some(tvs) = dec.ty_var_seq() {
        st.err(tvs.syntax(), ErrorKind::NonSpecDecSyntax);
      }
      let iter = dec.val_binds().filter_map(|val_bind| {
        if let Some(eq_exp) = val_bind.eq_exp() {
          st.err(eq_exp.syntax(), ErrorKind::NonSpecDecSyntax);
        }
        if let Some(rec_kw) = val_bind.rec_kw() {
          st.err_tok(&rec_kw, ErrorKind::NonSpecDecSyntax);
        }
        match val_bind.pat()? {
          ast::Pat::TypedPat(ty_pat) => match ty_pat.pat()? {
            ast::Pat::ConPat(con_pat) => {
              if let Some(op_kw) = con_pat.op_kw() {
                st.err_tok(&op_kw, ErrorKind::NonSpecDecSyntax);
              }
              if let Some(pat) = con_pat.pat() {
                st.err(pat.syntax(), ErrorKind::NonSpecDecSyntax);
              }
              let path = con_pat.path()?;
              let mut iter = path.name_star_eq_dots();
              let fst = iter.next()?;
              if iter.next().is_some() {
                st.err(path.syntax(), ErrorKind::NonSpecDecSyntax);
              }
              let name = str_util::Name::new(fst.name_star_eq()?.token.text());
              let ty = ty::get(st, ty_pat.ty());
              Some(sml_hir::ValDesc { name, ty })
            }
            pat => {
              st.err(pat.syntax(), ErrorKind::NonSpecDecSyntax);
              None
            }
          },
          pat => {
            st.err(pat.syntax(), ErrorKind::NonSpecDecSyntax);
            None
          }
        }
      });
      let val = sml_hir::Spec::Val(vec![], iter.collect());
      vec![st.spec(val, ptr)]
    }
    ast::DecOne::TyDec(dec) => {
      if !st.lang().dec.type_ {
        st.err(dec.syntax(), ErrorKind::Disallowed(Disallowed::Dec("`type`")));
      }
      let f = match dec.ty_head() {
        None => return vec![],
        Some(head) => match head.kind {
          ast::TyHeadKind::TypeKw => sml_hir::Spec::Ty,
          ast::TyHeadKind::EqtypeKw => sml_hir::Spec::EqTy,
        },
      };
      let iter = dec.ty_binds().filter_map(|ty_desc| {
        let ty_vars = ty::var_seq(st, ty_desc.ty_var_seq());
        let name = get_name(ty_desc.name())?;
        let mut ret = f(sml_hir::TyDesc { name: name.clone(), ty_vars: ty_vars.clone() });
        if let Some(ty) = ty_desc.eq_ty() {
          let ty = ty::get(st, ty.ty());
          let spec_idx = st.spec(ret, ptr);
          let sig_exp = st.sig_exp(sml_hir::SigExp::Spec(vec![spec_idx]), ptr);
          let where_kind = sml_hir::WhereKind::Type(ty_vars, sml_path::Path::one(name), ty);
          let sig_exp = st.sig_exp(sml_hir::SigExp::Where(sig_exp, where_kind), ptr);
          ret = sml_hir::Spec::Include(sig_exp);
        }
        Some(st.spec(ret, ptr))
      });
      iter.collect()
    }
    ast::DecOne::DatDec(dec) => {
      if !st.lang().dec.datatype {
        st.err(dec.syntax(), ErrorKind::Disallowed(Disallowed::Dec("`datatype`")));
      }
      let d_binds = dat_binds(st, dec.dat_binds());
      let t_binds = match dec.with_type() {
        Some(with_type) => {
          if !st.lang().successor_ml.sig_withtype {
            let e = ErrorKind::Disallowed(Disallowed::SuccMl("`withtype` in specifications"));
            st.err(with_type.syntax(), e);
          }
          ty_binds(st, with_type.ty_binds())
        }
        None => Vec::new(),
      };
      let dat = sml_hir::Spec::Datatype(d_binds, t_binds);
      vec![st.spec(dat, ptr)]
    }
    ast::DecOne::DatCopyDec(dec) => {
      if !st.lang().dec.datatype_copy {
        st.err(dec.syntax(), ErrorKind::Disallowed(Disallowed::Dec("`datatype` copy")));
      }
      get_name(dec.name())
        .zip(dec.path().and_then(get_path))
        .map(|(name, path)| vec![st.spec(sml_hir::Spec::DatatypeCopy(name, path), ptr)])
        .unwrap_or_default()
    }
    ast::DecOne::ExDec(dec) => {
      if !st.lang().dec.exception {
        st.err(dec.syntax(), ErrorKind::Disallowed(Disallowed::Dec("`exception`")));
      }
      let iter = dec.ex_binds().filter_map(|ex_bind| {
        let name = str_util::Name::new(ex_bind.name_star_eq()?.token.text());
        let ty = ex_bind.ex_bind_inner().map(|inner| match inner {
          ast::ExBindInner::OfTy(of_ty) => ty::get(st, of_ty.ty()),
          ast::ExBindInner::EqExp(eq_exp) => {
            st.err(eq_exp.syntax(), ErrorKind::NonSpecDecSyntax);
            None
          }
        });
        Some(st.spec(sml_hir::Spec::Exception(sml_hir::ExDesc { name, ty }), ptr))
      });
      iter.collect()
    }
    ast::DecOne::StructureDec(dec) => {
      if !st.lang().dec.structure {
        st.err(dec.syntax(), ErrorKind::Disallowed(Disallowed::Dec("`structure`")));
      }
      let iter = dec.str_binds().filter_map(|str_bind| {
        if let Some(eq_str_exp) = str_bind.eq_str_exp() {
          st.err(eq_str_exp.syntax(), ErrorKind::NonSpecDecSyntax);
        }
        let name = get_name(str_bind.name())?;
        let sig_exp = match str_bind.ascription_tail() {
          Some(tail) => match tail.ascription() {
            Some(asc) => match asc.kind {
              ast::AscriptionKind::Colon => get_sig_exp(st, tail.sig_exp()),
              ast::AscriptionKind::ColonGt => {
                st.err_tok(&asc.token, ErrorKind::NonSpecDecSyntax);
                None
              }
            },
            None => {
              st.err(tail.syntax(), ErrorKind::NonSpecDecSyntax);
              None
            }
          },
          None => {
            st.err(str_bind.syntax(), ErrorKind::NonSpecDecSyntax);
            None
          }
        };
        let spec = sml_hir::Spec::Str(sml_hir::StrDesc { name, sig_exp });
        Some(st.spec(spec, ptr))
      });
      iter.collect()
    }
    ast::DecOne::IncludeDec(dec) => {
      if !st.lang().dec.include {
        st.err(dec.syntax(), ErrorKind::Disallowed(Disallowed::Dec("`include`")));
      }
      let iter = dec.sig_exps().map(|x| {
        let spec = sml_hir::Spec::Include(get_sig_exp(st, Some(x)));
        st.spec(spec, ptr)
      });
      let specs: Vec<_> = iter.collect();
      if specs.is_empty() {
        st.err(dec.syntax(), ErrorKind::RequiresOperand);
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
      st.err(dec.syntax(), ErrorKind::NotSpec);
      vec![]
    }
  }
}

fn ascription_tail(
  st: &mut St<'_>,
  tail: Option<ast::AscriptionTail>,
) -> (sml_hir::Ascription, sml_hir::SigExpIdx) {
  let kind = tail.as_ref().and_then(sml_syntax::ast::AscriptionTail::ascription).map_or(
    sml_hir::Ascription::Transparent,
    |x| match x.kind {
      ast::AscriptionKind::Colon => sml_hir::Ascription::Transparent,
      ast::AscriptionKind::ColonGt => sml_hir::Ascription::Opaque,
    },
  );
  (kind, get_sig_exp(st, tail.and_then(|x| x.sig_exp())))
}

fn with_ascription_tail(
  st: &mut St<'_>,
  str_exp: Option<ast::StrExp>,
  tail: Option<ast::AscriptionTail>,
) -> sml_hir::StrExpIdx {
  let str_exp = str_exp?;
  let ptr = SyntaxNodePtr::new(str_exp.syntax());
  let mut ret = get_str_exp(st, Some(str_exp));
  if let Some(tail) = tail {
    let (kind, sig_exp) = ascription_tail(st, Some(tail));
    let asc = sml_hir::StrExp::Ascription(ret, kind, sig_exp);
    ret = st.str_exp(asc, ptr);
  }
  ret
}

pub(crate) fn get<I>(st: &mut St<'_>, iter: I) -> sml_hir::DecSeq
where
  I: Iterator<Item = ast::Dec>,
{
  get_dec_flavor(st, iter, get_one)
}

fn get_one(st: &mut St<'_>, dec: ast::DecOne) -> Option<sml_hir::DecIdx> {
  let ptr = SyntaxNodePtr::new(dec.syntax());
  let ret = match dec {
    ast::DecOne::HoleDec(_) => {
      st.err(dec.syntax(), ErrorKind::DecHole);
      return None;
    }
    ast::DecOne::ValDec(dec) => {
      if !st.lang().dec.val {
        st.err(dec.syntax(), ErrorKind::Disallowed(Disallowed::Dec("`val`")));
      }
      if st.is_top_level() && st.file_kind().is_sig_or_fun() {
        st.err(dec.syntax(), ErrorKind::DiscouragedTopDecKindForSigFun);
      }
      let ty_vars = ty::var_seq(st, dec.ty_var_seq());
      let iter = dec.val_binds().map(|val_bind| {
        let exp = val_bind.eq_exp().and_then(|x| x.exp());
        if exp.is_none() {
          st.err(val_bind.syntax(), ErrorKind::MissingRhs(MissingRhs::Dec));
        }
        sml_hir::ValBind {
          rec: val_bind.rec_kw().is_some(),
          pat: pat::get(st, None, val_bind.pat()),
          exp: exp::get(st, exp),
        }
      });
      sml_hir::Dec::Val(ty_vars, iter.collect(), sml_hir::ValFlavor::Val)
    }
    ast::DecOne::FunDec(dec) => {
      if !st.lang().dec.fun {
        st.err(dec.syntax(), ErrorKind::Disallowed(Disallowed::Dec("`fun`")));
      }
      if st.is_top_level() && st.file_kind().is_sig_or_fun() {
        st.err(dec.syntax(), ErrorKind::DiscouragedTopDecKindForSigFun);
      }
      let ty_vars = ty::var_seq(st, dec.ty_var_seq());
      let iter = dec.fun_binds().map(|fun_bind| {
        if let (false, Some(bar)) = (st.lang().successor_ml.opt_bar, fun_bind.bar()) {
          let e =
            ErrorKind::Disallowed(Disallowed::SuccMl("preceding `|` before first `fun` case"));
          st.err_tok(&bar, e);
        }
        let ptr = SyntaxNodePtr::new(fun_bind.syntax());
        let mut name = None::<sml_syntax::kind::SyntaxToken>;
        let mut num_pats = None::<usize>;
        let iter = fun_bind.fun_bind_cases().map(|case| {
          let mut pats = Vec::<sml_hir::PatIdx>::with_capacity(1);
          let head_name = case
            .fun_bind_case_head()
            .and_then(|head| match head {
              ast::FunBindCaseHead::PrefixFunBindCaseHead(head) => head.name_star_eq(),
              ast::FunBindCaseHead::InfixFunBindCaseHead(head) => {
                let lhs = head.lhs();
                let rhs = head.rhs();
                let tup = tuple([pat::get(st, None, lhs), pat::get(st, None, rhs)]);
                pats.push(st.pat(tup, ptr));
                head.name_star_eq()
              }
            })
            .map(|x| x.token);
          match (name.as_ref(), head_name) {
            (_, None) => {}
            (None, Some(head_name)) => name = Some(head_name),
            (Some(name), Some(head_name)) => {
              if name.text() != head_name.text() {
                let ek = ErrorKind::FunBindMismatchedName(
                  name.text().to_owned(),
                  head_name.text().to_owned(),
                );
                st.err_tok(&head_name, ek);
              }
            }
          }
          pats.extend(case.pats().map(|pat| pat::get(st, None, Some(pat))));
          match num_pats {
            None => num_pats = Some(pats.len()),
            Some(num_pats) => {
              if num_pats != pats.len() {
                st.err_wrong_num_pats(&case, num_pats, pats.len());
              }
            }
          }
          let pat =
            if pats.len() == 1 { pats.pop().unwrap() } else { st.pat(pat::tuple(pats), ptr) };
          let ty = case.ty_annotation().map(|ty_ann| {
            forbid_opaque_asc(st, ty_ann.ascription());
            ty::get(st, ty_ann.ty())
          });
          let body = case.eq_exp().and_then(|x| x.exp());
          if body.is_none() {
            st.err(dec.syntax(), ErrorKind::MissingRhs(MissingRhs::Dec));
          }
          if let Some(name) = &name {
            st.push_fun_name(str_util::Name::new(name.text()));
          }
          let body = body.and_then(|body| {
            let ptr = SyntaxNodePtr::new(body.syntax());
            let mut body = exp::get(st, Some(body));
            if let Some(ty) = ty {
              body = st.exp(sml_hir::Exp::Typed(body, ty, sml_hir::TypedFlavor::Fun), ptr);
            }
            body
          });
          if name.is_some() {
            st.pop_fun_name();
          }
          sml_hir::Arm { pat, exp: body }
        });
        let arms: Vec<_> = iter.collect();
        // not the greatest, since we have no body at all if the ptrs are None. but if they were
        // both None, then something's very strange about the fun_bind_cases anyway.
        if num_pats == Some(0) {
          st.err(dec.syntax(), ErrorKind::EmptyFun);
        }
        let exp = {
          let arg_names: Vec<_> = (0..num_pats.unwrap_or(1)).map(|_| st.fresh()).collect();
          let mut arg_exprs = arg_names.iter().map(|name| st.exp(exp::name(name.as_str()), ptr));
          let head = if arg_exprs.len() == 1 {
            arg_exprs.next().unwrap()
          } else {
            let tup = exp::tuple(arg_exprs);
            st.exp(tup, ptr)
          };
          let flavor = sml_hir::FnFlavor::FunCase { tuple: num_pats.is_some_and(|x| x != 1) };
          let case = exp::case(st, head, arms, ptr, flavor);
          arg_names.into_iter().rev().fold(st.exp(case, ptr), |body, name| {
            let pat = st.pat(pat::name(name.as_str()), ptr);
            let arm = sml_hir::Arm { pat, exp: body };
            st.exp(sml_hir::Exp::Fn(vec![arm], sml_hir::FnFlavor::FunArg), ptr)
          })
        };
        sml_hir::ValBind {
          rec: true,
          pat: name.and_then(|name| st.pat(pat::name(name.text()), ptr)),
          exp,
        }
      });
      sml_hir::Dec::Val(ty_vars, iter.collect(), sml_hir::ValFlavor::Fun)
    }
    ast::DecOne::TyDec(dec) => {
      if !st.lang().dec.type_ {
        st.err(dec.syntax(), ErrorKind::Disallowed(Disallowed::Dec("`type`")));
      }
      if st.is_top_level() && st.file_kind().is_sig_or_fun() {
        st.err(dec.syntax(), ErrorKind::DiscouragedTopDecKindForSigFun);
      }
      let hd = dec.ty_head()?;
      match hd.kind {
        ast::TyHeadKind::TypeKw => {}
        ast::TyHeadKind::EqtypeKw => st.err_tok(&hd.token, ErrorKind::InvalidEqtype),
      }
      sml_hir::Dec::Ty(ty_binds(st, dec.ty_binds()))
    }
    ast::DecOne::DatDec(dec) => {
      if !st.lang().dec.datatype {
        st.err(dec.syntax(), ErrorKind::Disallowed(Disallowed::Dec("`datatype`")));
      }
      if st.is_top_level() && st.file_kind().is_sig_or_fun() {
        st.err(dec.syntax(), ErrorKind::DiscouragedTopDecKindForSigFun);
      }
      let d_binds = dat_binds(st, dec.dat_binds());
      let t_binds = ty_binds(st, dec.with_type().into_iter().flat_map(|x| x.ty_binds()));
      sml_hir::Dec::Datatype(d_binds, t_binds)
    }
    ast::DecOne::DatCopyDec(dec) => {
      if !st.lang().dec.datatype_copy {
        st.err(dec.syntax(), ErrorKind::Disallowed(Disallowed::Dec("`datatype` copy")));
      }
      if st.is_top_level() && st.file_kind().is_sig_or_fun() {
        st.err(dec.syntax(), ErrorKind::DiscouragedTopDecKindForSigFun);
      }
      sml_hir::Dec::DatatypeCopy(get_name(dec.name())?, get_path(dec.path()?)?)
    }
    ast::DecOne::AbstypeDec(dec) => {
      if st.is_top_level() && st.file_kind().is_sig_or_fun() {
        st.err(dec.syntax(), ErrorKind::DiscouragedTopDecKindForSigFun);
      }
      let d_binds = dat_binds(st, dec.dat_binds());
      let t_binds = ty_binds(st, dec.with_type().into_iter().flat_map(|x| x.ty_binds()));
      let inner = get(st, dec.decs());
      sml_hir::Dec::Abstype(d_binds, t_binds, inner)
    }
    ast::DecOne::ExDec(dec) => {
      if !st.lang().dec.exception {
        st.err(dec.syntax(), ErrorKind::Disallowed(Disallowed::Dec("`exception`")));
      }
      if st.is_top_level() && st.file_kind().is_sig_or_fun() {
        st.err(dec.syntax(), ErrorKind::DiscouragedTopDecKindForSigFun);
      }
      let iter = dec.ex_binds().filter_map(|ex_bind| {
        let name = str_util::Name::new(ex_bind.name_star_eq()?.token.text());
        let ret = match ex_bind.ex_bind_inner() {
          None => sml_hir::ExBind::New(name, None),
          Some(ast::ExBindInner::OfTy(x)) => sml_hir::ExBind::New(name, Some(ty::get(st, x.ty()))),
          Some(ast::ExBindInner::EqExp(x)) => match x.exp()? {
            ast::Exp::PathExp(exp) => sml_hir::ExBind::Copy(name, get_path(exp.path()?)?),
            exp => {
              st.err(exp.syntax(), ErrorKind::ExceptionCopyRhsNotPath);
              sml_hir::ExBind::New(name, None)
            }
          },
        };
        Some(ret)
      });
      sml_hir::Dec::Exception(iter.collect())
    }
    ast::DecOne::LocalDec(dec) => {
      if !st.lang().dec.local {
        st.err(dec.syntax(), ErrorKind::Disallowed(Disallowed::Dec("`local`")));
      }
      st.inc_level();
      let fst = get(st, dec.local_dec_hd().into_iter().flat_map(|x| x.decs()));
      st.dec_level();
      let snd = get(st, dec.local_dec_tl().into_iter().flat_map(|x| x.decs()));
      if fst.is_empty() {
        st.err(dec.syntax(), ErrorKind::EmptyLocal);
      }
      sml_hir::Dec::Local(fst, snd)
    }
    ast::DecOne::OpenDec(dec) => {
      if !st.lang().dec.open {
        st.err(dec.syntax(), ErrorKind::Disallowed(Disallowed::Dec("`open`")));
      }
      if st.is_top_level() {
        st.err(dec.syntax(), ErrorKind::TopLevelOpen);
      }
      let paths: Vec<_> = dec.paths().filter_map(get_path).collect();
      if paths.is_empty() {
        st.err(dec.syntax(), ErrorKind::RequiresOperand);
      }
      sml_hir::Dec::Open(paths)
    }
    ast::DecOne::InfixDec(_) | ast::DecOne::InfixrDec(_) | ast::DecOne::NonfixDec(_) => {
      if !st.lang().dec.fixity {
        let e = ErrorKind::Disallowed(Disallowed::Dec("`infix`, `infixr`, or `nonfix`"));
        st.err(dec.syntax(), e);
      }
      if st.is_top_level() && st.file_kind().is_sig_or_fun() {
        st.err(dec.syntax(), ErrorKind::DiscouragedTopDecKindForSigFun);
      }
      return None;
    }
    ast::DecOne::DoDec(ref inner) => {
      if !st.lang().successor_ml.do_dec {
        st.err(dec.syntax(), ErrorKind::Disallowed(Disallowed::SuccMl("`do` declarations")));
      }
      if st.is_top_level() && st.file_kind().is_sig_or_fun() {
        st.err(dec.syntax(), ErrorKind::DiscouragedTopDecKindForSigFun);
      }
      let bind = sml_hir::ValBind {
        rec: false,
        pat: st.pat(pat::tuple([]), ptr),
        exp: exp::get(st, inner.exp()),
      };
      sml_hir::Dec::Val(Vec::new(), vec![bind], sml_hir::ValFlavor::Do)
    }
    ast::DecOne::StructureDec(_)
    | ast::DecOne::SignatureDec(_)
    | ast::DecOne::FunctorDec(_)
    | ast::DecOne::IncludeDec(_) => {
      st.err(dec.syntax(), ErrorKind::DecNotAllowedHere);
      return None;
    }
    ast::DecOne::ExpDec(_) => {
      st.err(dec.syntax(), ErrorKind::ExpNotAllowedHere);
      return None;
    }
  };
  Some(st.dec(ret, ptr))
}

fn dat_binds<I>(st: &mut St<'_>, iter: I) -> Vec<sml_hir::DatBind>
where
  I: Iterator<Item = ast::DatBind>,
{
  let iter = iter.filter_map(|dat_bind| {
    let cons: Vec<sml_hir::ConBind> = match dat_bind.eq_con_binds() {
      None => {
        st.err(dat_bind.syntax(), ErrorKind::MissingRhs(MissingRhs::Dec));
        vec![]
      }
      Some(eq_con_binds) => {
        if let (false, Some(bar)) = (st.lang().successor_ml.opt_bar, eq_con_binds.bar()) {
          let e = ErrorKind::Disallowed(Disallowed::SuccMl(
            "preceding `|` before first `datatype` constructor",
          ));
          st.err_tok(&bar, e);
        }
        let iter = eq_con_binds.con_binds().filter_map(|con_bind| {
          Some(sml_hir::ConBind {
            name: str_util::Name::new(con_bind.name_star_eq()?.token.text()),
            ty: con_bind.of_ty().map(|x| ty::get(st, x.ty())),
          })
        });
        iter.collect()
      }
    };
    Some(sml_hir::DatBind {
      ty_vars: ty::var_seq(st, dat_bind.ty_var_seq()),
      name: get_name(dat_bind.name())?,
      cons,
    })
  });
  iter.collect()
}

fn ty_binds<I>(st: &mut St<'_>, iter: I) -> Vec<sml_hir::TyBind>
where
  I: Iterator<Item = ast::TyBind>,
{
  let iter = iter.filter_map(|ty_bind| {
    let name = get_name(ty_bind.name())?;
    let ty = ty_bind.eq_ty().and_then(|x| x.ty());
    if ty.is_none() {
      st.err(ty_bind.syntax(), ErrorKind::MissingRhs(MissingRhs::Dec));
    }
    Some(sml_hir::TyBind {
      ty_vars: ty::var_seq(st, ty_bind.ty_var_seq()),
      name,
      ty: ty::get(st, ty),
    })
  });
  iter.collect()
}
