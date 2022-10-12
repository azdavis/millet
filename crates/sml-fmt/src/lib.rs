//! Format SML files.
//!
//! Very WIP.
//!
//! - Doesn't handle comments.
//! - Doesn't handle many, many actual language constructs.
//! - No attempt is made to restrict lines to a reasonable length.
//!
//! Basically: don't use this.

#![deny(missing_debug_implementations, missing_docs, rust_2018_idioms)]

use sml_syntax::ast;
use std::fmt;

/// Returns a value that displays the root.
pub fn display_root(root: &ast::Root) -> impl fmt::Display + '_ {
  DisplayRoot(root)
}

struct DisplayRoot<'a>(&'a ast::Root);

impl fmt::Display for DisplayRoot<'_> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    self.0.dec().and_then(|d| get_dec(f, Cfg::default(), d)).ok_or(fmt::Error)
  }
}

type Res = Option<()>;

#[derive(Debug, Default, Clone, Copy)]
struct Cfg {
  indent: usize,
}

impl Cfg {
  fn indented(self) -> Self {
    Self { indent: self.indent + 1 }
  }

  fn output_indent(&self, f: &mut fmt::Formatter<'_>) -> Res {
    for _ in 0..self.indent {
      output(f, "  ")?;
    }
    Some(())
  }
}

fn output(f: &mut fmt::Formatter<'_>, s: &str) -> Res {
  f.write_str(s).ok()
}

fn get_dec(f: &mut fmt::Formatter<'_>, cfg: Cfg, dec: ast::Dec) -> Res {
  sep_with_lines(f, cfg, "", dec.dec_in_seqs(), |f, dec_in_seq| {
    get_dec_one(f, cfg, dec_in_seq.dec_one()?)?;
    if dec_in_seq.semicolon().is_some() {
      output(f, ";")?;
    }
    Some(())
  })
}

/// TODO rm
fn nothing() -> Res {
  Some(())
}

fn get_dec_one(f: &mut fmt::Formatter<'_>, cfg: Cfg, dec: ast::DecOne) -> Res {
  match dec {
    ast::DecOne::HoleDec(_) => output(f, "..."),
    ast::DecOne::ValDec(dec) => {
      output(f, "val ")?;
      sep_with_lines(f, cfg, "and ", dec.val_binds(), |f, val_bind| {
        get_pat(f, val_bind.pat()?)?;
        output(f, " = ")?;
        get_exp(f, cfg, val_bind.exp()?)
      })
    }
    ast::DecOne::FunDec(dec) => {
      output(f, "fun ")?;
      ty_var_seq(f, dec.ty_var_seq())?;
      sep_with_lines(f, cfg, "and ", dec.fun_binds(), |f, fun_bind| {
        sep_with_lines(f, cfg.indented(), "| ", fun_bind.fun_bind_cases(), |f, fun_bind_case| {
          match fun_bind_case.fun_bind_case_head()? {
            ast::FunBindCaseHead::PrefixFunBindCaseHead(head) => {
              if head.op_kw().is_some() {
                output(f, "op ")?;
              }
              output(f, head.name_star_eq()?.token.text())?;
            }
            ast::FunBindCaseHead::InfixFunBindCaseHead(head) => {
              let parens = head.l_round().is_some();
              if parens {
                output(f, "(")?;
              }
              get_pat(f, head.lhs()?)?;
              output(f, " ")?;
              output(f, head.name_star_eq()?.token.text())?;
              output(f, " ")?;
              get_pat(f, head.lhs()?)?;
              if parens {
                output(f, ")")?;
              }
            }
          }
          output(f, " ")?;
          sep(f, " ", fun_bind_case.pats(), get_pat)?;
          ty_annotation(f, fun_bind_case.ty_annotation())?;
          output(f, " =")?;
          get_body_exp(f, cfg, fun_bind_case.exp()?)
        })
      })
    }
    ast::DecOne::TyDec(dec) => {
      output(f, "type ")?;
      ty_binds(f, cfg, dec.ty_binds())
    }
    ast::DecOne::DatDec(dec) => {
      output(f, "datatype ")?;
      sep_with_lines(f, cfg, "and ", dec.dat_binds(), |f, dat_bind| {
        ty_var_seq(f, dat_bind.ty_var_seq())?;
        output(f, dat_bind.name()?.text())?;
        output(f, " =\n")?;
        cfg.indented().output_indent(f)?;
        sep_with_lines(f, cfg, "| ", dat_bind.con_binds(), |f, con_bind| {
          output(f, con_bind.name_star_eq()?.token.text())?;
          if let Some(of_ty) = con_bind.of_ty() {
            output(f, " of ")?;
            get_ty(f, of_ty.ty()?)?;
          }
          Some(())
        })
      })?;
      if let Some(withtype) = dec.with_type() {
        output(f, " withtype ")?;
        ty_binds(f, cfg, withtype.ty_binds())?;
      }
      Some(())
    }
    ast::DecOne::DatCopyDec(dec) => {
      output(f, "datatype ")?;
      output(f, dec.name()?.text())?;
      output(f, " = datatype ")?;
      path(f, dec.path()?)
    }
    ast::DecOne::AbstypeDec(_) => nothing(),
    ast::DecOne::ExDec(dec) => {
      output(f, "exception ")?;
      sep_with_lines(f, cfg, "and ", dec.ex_binds(), |f, ex_bind| {
        output(f, ex_bind.name_star_eq()?.token.text())?;
        match ex_bind.ex_bind_inner() {
          Some(inner) => match inner {
            ast::ExBindInner::OfTy(of_ty) => {
              output(f, " of ")?;
              get_ty(f, of_ty.ty()?)
            }
            ast::ExBindInner::EqPath(eq_path) => {
              output(f, " = ")?;
              path(f, eq_path.path()?)
            }
          },
          None => Some(()),
        }
      })
    }
    ast::DecOne::OpenDec(dec) => {
      output(f, "open ")?;
      sep(f, " ", dec.paths(), path)
    }
    ast::DecOne::InfixDec(dec) => {
      output(f, "infix ")?;
      if let Some(int_lit) = dec.int_lit() {
        output(f, int_lit.text())?;
      }
      sep(f, " ", dec.name_star_eqs(), |f, n| output(f, n.token.text()))
    }
    ast::DecOne::InfixrDec(dec) => {
      output(f, "infixr ")?;
      if let Some(int_lit) = dec.int_lit() {
        output(f, int_lit.text())?;
      }
      sep(f, " ", dec.name_star_eqs(), |f, n| output(f, n.token.text()))
    }
    ast::DecOne::NonfixDec(dec) => {
      output(f, "nonfix ")?;
      sep(f, " ", dec.name_star_eqs(), |f, n| output(f, n.token.text()))
    }
    ast::DecOne::DoDec(dec) => {
      output(f, "do ")?;
      get_exp(f, cfg, dec.exp()?)
    }
    ast::DecOne::LocalDec(dec) => in_end(
      f,
      cfg,
      "local",
      |f, cfg| get_dec(f, cfg, dec.local_dec()?),
      |f, cfg| get_dec(f, cfg, dec.in_dec()?),
    ),
    ast::DecOne::StructureDec(dec) => {
      output(f, "structure ")?;
      sep_with_lines(f, cfg, "and ", dec.str_binds(), |f, str_bind| {
        output(f, str_bind.name()?.text())?;
        if let Some(tail) = str_bind.ascription_tail() {
          ascription_tail(f, cfg, tail)?;
        }
        output(f, " = ")?;
        get_str_exp(f, cfg, str_bind.str_exp()?)
      })
    }
    ast::DecOne::SignatureDec(dec) => {
      output(f, "signature ")?;
      sep_with_lines(f, cfg, "and ", dec.sig_binds(), |f, sig_bind| {
        output(f, sig_bind.name()?.text())?;
        output(f, " = ")?;
        get_sig_exp(f, cfg, sig_bind.sig_exp()?)
      })
    }
    ast::DecOne::FunctorDec(_) => nothing(),
    ast::DecOne::ExpDec(dec) => get_exp(f, cfg, dec.exp()?),
  }
}

fn ascription_tail(f: &mut fmt::Formatter<'_>, cfg: Cfg, tail: ast::AscriptionTail) -> Res {
  output(f, " ")?;
  output(f, tail.ascription()?.token.text())?;
  get_sig_exp(f, cfg, tail.sig_exp()?)
}

fn get_str_exp(f: &mut fmt::Formatter<'_>, cfg: Cfg, str_exp: ast::StrExp) -> Res {
  match str_exp {
    ast::StrExp::StructStrExp(exp) => {
      output(f, "struct\n")?;
      let new_cfg = cfg.indented();
      new_cfg.output_indent(f)?;
      get_dec(f, new_cfg, exp.dec()?)?;
      output(f, "\n")?;
      cfg.output_indent(f)?;
      output(f, "end")
    }
    ast::StrExp::PathStrExp(exp) => path(f, exp.path()?),
    ast::StrExp::AscriptionStrExp(exp) => {
      get_str_exp(f, cfg, exp.str_exp()?)?;
      ascription_tail(f, cfg, exp.ascription_tail()?)
    }
    ast::StrExp::AppStrExp(exp) => {
      output(f, exp.name()?.text())?;
      output(f, "(")?;
      match exp.app_str_exp_arg()? {
        ast::AppStrExpArg::AppStrExpArgStrExp(arg) => get_str_exp(f, cfg, arg.str_exp()?)?,
        ast::AppStrExpArg::Dec(arg) => get_dec(f, cfg, arg)?,
      }
      output(f, ")")
    }
    ast::StrExp::LetStrExp(exp) => in_end(
      f,
      cfg,
      "let",
      |f, cfg| get_str_exp(f, cfg, exp.str_exp()?),
      |f, cfg| get_dec(f, cfg, exp.dec()?),
    ),
  }
}

fn get_sig_exp(f: &mut fmt::Formatter<'_>, cfg: Cfg, sig_exp: ast::SigExp) -> Res {
  match sig_exp {
    ast::SigExp::SigSigExp(exp) => {
      output(f, "sig\n")?;
      let new_cfg = cfg.indented();
      cfg.output_indent(f)?;
      get_spec(f, new_cfg, exp.spec()?)?;
      output(f, "\n")?;
      cfg.output_indent(f)?;
      output(f, "end")
    }
    ast::SigExp::NameSigExp(exp) => output(f, exp.name()?.text()),
    ast::SigExp::WhereTypeSigExp(exp) => {
      get_sig_exp(f, cfg, exp.sig_exp()?)?;
      output(f, " where type ")?;
      ty_var_seq(f, exp.ty_var_seq())?;
      path(f, exp.path()?)?;
      output(f, " = ")?;
      get_ty(f, exp.ty()?)
    }
    ast::SigExp::WhereSigExp(exp) => {
      get_sig_exp(f, cfg, exp.sig_exp()?)?;
      output(f, " where ")?;
      path(f, exp.lhs()?)?;
      output(f, " = ")?;
      path(f, exp.rhs()?)
    }
  }
}

fn get_spec(f: &mut fmt::Formatter<'_>, cfg: Cfg, spec: ast::Spec) -> Res {
  sep_with_lines(f, cfg, "", spec.spec_with_tail_in_seqs(), |f, spec| {
    let spec_with_tail = spec.spec_with_tail()?;
    sep_with_lines(f, cfg, "", spec_with_tail.spec_in_seqs(), |f, spec| {
      get_spec_one(f, cfg, spec.spec_one()?)?;
      if spec.semicolon().is_some() {
        output(f, ";")?;
      }
      Some(())
    })?;
    sep_with_lines(f, cfg, "", spec_with_tail.sharing_tails(), |f, sharing| {
      output(f, "sharing type ")?;
      sep(f, " = ", sharing.path_eqs(), |f, p| path(f, p.path()?))
    })?;
    if spec.semicolon().is_some() {
      output(f, ";")?;
    }
    Some(())
  })
}

fn get_spec_one(_: &mut fmt::Formatter<'_>, _: Cfg, spec: ast::SpecOne) -> Res {
  match spec {
    ast::SpecOne::ValSpec(_) => nothing(),
    ast::SpecOne::TySpec(_) => nothing(),
    ast::SpecOne::EqTySpec(_) => nothing(),
    ast::SpecOne::DatSpec(_) => nothing(),
    ast::SpecOne::DatCopySpec(_) => nothing(),
    ast::SpecOne::ExSpec(_) => nothing(),
    ast::SpecOne::StrSpec(_) => nothing(),
    ast::SpecOne::IncludeSpec(_) => nothing(),
  }
}

fn ty_annotation(f: &mut fmt::Formatter<'_>, ty_ann: Option<ast::TyAnnotation>) -> Res {
  match ty_ann {
    Some(ty_ann) => {
      output(f, " : ")?;
      get_ty(f, ty_ann.ty()?)
    }
    None => Some(()),
  }
}

fn ty_binds<I>(f: &mut fmt::Formatter<'_>, cfg: Cfg, iter: I) -> Res
where
  I: Iterator<Item = ast::TyBind>,
{
  sep_with_lines(f, cfg, "and ", iter, |f, ty_bind| {
    ty_var_seq(f, ty_bind.ty_var_seq())?;
    output(f, ty_bind.name()?.text())?;
    output(f, " = ")?;
    get_ty(f, ty_bind.ty()?)
  })
}

fn ty_var_seq(f: &mut fmt::Formatter<'_>, tvs: Option<ast::TyVarSeq>) -> Res {
  let tvs = match tvs {
    Some(x) => x,
    None => return Some(()),
  };
  let parens = tvs.l_round().is_some();
  if parens {
    output(f, "(")?;
  }
  sep(f, ", ", tvs.ty_var_args(), |f, tv| output(f, tv.ty_var()?.text()))?;
  if parens {
    output(f, ")")?;
  }
  output(f, " ")?;
  Some(())
}

fn get_body_exp(f: &mut fmt::Formatter<'_>, cfg: Cfg, exp: ast::Exp) -> Res {
  let needs_newline = matches!(
    exp,
    ast::Exp::SeqExp(_)
      | ast::Exp::LetExp(_)
      | ast::Exp::HandleExp(_)
      | ast::Exp::IfExp(_)
      | ast::Exp::WhileExp(_)
      | ast::Exp::CaseExp(_)
  );
  if needs_newline {
    output(f, "\n");
    let new_cfg = cfg.indented();
    new_cfg.output_indent(f)?;
    get_exp(f, new_cfg, exp)
  } else {
    output(f, " ")?;
    get_exp(f, cfg, exp)
  }
}

fn get_exp(f: &mut fmt::Formatter<'_>, cfg: Cfg, exp: ast::Exp) -> Res {
  match exp {
    ast::Exp::HoleExp(_) => output(f, "..."),
    ast::Exp::WildcardExp(_) => output(f, "_"),
    ast::Exp::OpAndalsoExp(_) => output(f, "op andalso"),
    ast::Exp::OpOrelseExp(_) => output(f, "op orelse"),
    ast::Exp::SConExp(exp) => output(f, exp.s_con()?.token.text()),
    ast::Exp::PathExp(exp) => {
      if exp.op_kw().is_some() {
        output(f, "op ")?;
      }
      path(f, exp.path()?)
    }
    ast::Exp::RecordExp(exp) => {
      output(f, "{")?;
      sep(f, ", ", exp.exp_rows(), |f, row| {
        output(f, row.lab()?.token.text())?;
        if let Some(eq_exp) = row.eq_exp() {
          output(f, " = ")?;
          get_exp(f, cfg, eq_exp.exp()?)?;
        }
        Some(())
      })?;
      output(f, "}")
    }
    ast::Exp::SelectorExp(exp) => {
      output(f, "#")?;
      output(f, exp.lab()?.token.text())
    }
    ast::Exp::ParenExp(exp) => {
      output(f, "(")?;
      get_exp(f, cfg, exp.exp()?)?;
      output(f, ")")
    }
    ast::Exp::TupleExp(exp) => {
      output(f, "(")?;
      exp_args(f, cfg, exp.exp_args())?;
      output(f, ")")
    }
    ast::Exp::ListExp(exp) => {
      output(f, "[")?;
      exp_args(f, cfg, exp.exp_args())?;
      output(f, "]")
    }
    ast::Exp::VectorExp(exp) => {
      output(f, "#[")?;
      exp_args(f, cfg, exp.list_exp()?.exp_args())?;
      output(f, "]")
    }
    ast::Exp::SeqExp(exp) => {
      output(f, "(\n")?;
      let new_cfg = cfg.indented();
      new_cfg.output_indent(f)?;
      exp_semi_seq(f, new_cfg, exp.exps_in_seq())?;
      output(f, "\n")?;
      cfg.output_indent(f)?;
      output(f, ")")
    }
    ast::Exp::LetExp(exp) => in_end(
      f,
      cfg,
      "let",
      |f, cfg| get_dec(f, cfg, exp.dec()?),
      |f, cfg| exp_semi_seq(f, cfg, exp.exps_in_seq()),
    ),
    ast::Exp::AppExp(exp) => {
      let func = exp.func()?;
      let arg = exp.arg()?;
      let func_is_bang = match &func {
        ast::Exp::PathExp(path) => {
          let mut iter = path.path()?.name_star_eq_dots();
          let fst_is_bang = iter.next()?.name_star_eq()?.token.text() == "!";
          fst_is_bang && iter.next().is_none()
        }
        _ => false,
      };
      let arg_is_symbolic = match &arg {
        ast::Exp::PathExp(path) => {
          let fst = path.path()?.name_star_eq_dots().next()?.name_star_eq()?;
          !fst.token.text().bytes().next()?.is_ascii_alphabetic()
        }
        _ => false,
      };
      get_exp(f, cfg, func)?;
      if !func_is_bang || arg_is_symbolic {
        output(f, " ")?;
      }
      get_exp(f, cfg, arg)
    }
    ast::Exp::InfixExp(exp) => {
      get_exp(f, cfg, exp.lhs()?)?;
      output(f, " ")?;
      output(f, exp.name_star_eq()?.token.text())?;
      output(f, " ")?;
      get_exp(f, cfg, exp.rhs()?)
    }
    ast::Exp::TypedExp(exp) => {
      get_exp(f, cfg, exp.exp()?)?;
      output(f, " : ")?;
      get_ty(f, exp.ty()?)
    }
    ast::Exp::AndalsoExp(exp) => {
      get_exp(f, cfg, exp.lhs()?)?;
      output(f, " andalso ")?;
      get_exp(f, cfg, exp.rhs()?)
    }
    ast::Exp::OrelseExp(exp) => {
      get_exp(f, cfg, exp.lhs()?)?;
      output(f, " orelse ")?;
      get_exp(f, cfg, exp.rhs()?)
    }
    ast::Exp::HandleExp(exp) => {
      get_exp(f, cfg, exp.exp()?)?;
      output(f, " handle\n")?;
      get_matcher(f, cfg, exp.matcher()?)
    }
    ast::Exp::RaiseExp(exp) => {
      output(f, "raise ")?;
      get_exp(f, cfg, exp.exp()?)
    }
    ast::Exp::IfExp(exp) => {
      output(f, "if ")?;
      get_exp(f, cfg, exp.cond()?)?;
      output(f, " then\n")?;
      let new_cfg = cfg.indented();
      new_cfg.output_indent(f)?;
      get_exp(f, new_cfg, exp.yes()?)?;
      output(f, "\n")?;
      cfg.output_indent(f)?;
      output(f, "else")?;
      let no = exp.no()?;
      if matches!(no, ast::Exp::IfExp(_)) {
        output(f, " ")?;
        get_exp(f, cfg, no)
      } else {
        output(f, "\n")?;
        new_cfg.output_indent(f)?;
        get_exp(f, new_cfg, no)
      }
    }
    ast::Exp::WhileExp(exp) => {
      output(f, "while")?;
      get_exp(f, cfg, exp.cond()?)?;
      output(f, " do ")?;
      get_exp(f, cfg, exp.body()?)
    }
    ast::Exp::CaseExp(exp) => {
      output(f, "case ")?;
      get_exp(f, cfg, exp.exp()?)?;
      output(f, " of\n")?;
      get_matcher(f, cfg, exp.matcher()?)
    }
    ast::Exp::FnExp(exp) => {
      output(f, "fn ")?;
      get_matcher(f, cfg, exp.matcher()?)
    }
  }
}

fn get_matcher(f: &mut fmt::Formatter<'_>, cfg: Cfg, matcher: ast::Matcher) -> Res {
  cfg.indented().output_indent(f)?;
  sep_with_lines(f, cfg, "| ", matcher.match_rules(), |f, arm| {
    get_pat(f, arm.pat()?)?;
    output(f, " =>")?;
    get_body_exp(f, cfg, arm.exp()?)
  })
}

fn get_pat(f: &mut fmt::Formatter<'_>, pat: ast::Pat) -> Res {
  match pat {
    ast::Pat::WildcardPat(_) => output(f, "_"),
    ast::Pat::SConPat(pat) => output(f, pat.s_con()?.token.text()),
    ast::Pat::ConPat(pat) => {
      if pat.op_kw().is_some() {
        output(f, "op ")?;
      }
      path(f, pat.path()?)?;
      if let Some(pat) = pat.pat() {
        output(f, " ")?;
        get_pat(f, pat)?;
      }
      Some(())
    }
    ast::Pat::RecordPat(pat) => {
      output(f, "{")?;
      sep(f, ", ", pat.pat_rows(), |f, row| match row.pat_row_inner()? {
        ast::PatRowInner::RestPatRow(_) => output(f, "..."),
        ast::PatRowInner::LabAndPatPatRow(row) => {
          output(f, row.lab()?.token.text())?;
          output(f, " = ")?;
          get_pat(f, row.pat()?)
        }
        ast::PatRowInner::LabPatRow(row) => {
          output(f, row.name_star_eq()?.token.text())?;
          ty_annotation(f, row.ty_annotation())?;
          if let Some(tail) = row.as_pat_tail() {
            output(f, " as ")?;
            get_pat(f, tail.pat()?)?;
          }
          Some(())
        }
      })?;
      output(f, "}")
    }
    ast::Pat::ParenPat(pat) => {
      output(f, "(")?;
      get_pat(f, pat.pat()?)?;
      output(f, ")")
    }
    ast::Pat::TuplePat(pat) => {
      output(f, "(")?;
      pat_args(f, pat.pat_args())?;
      output(f, ")")
    }
    ast::Pat::ListPat(pat) => {
      output(f, "[")?;
      pat_args(f, pat.pat_args())?;
      output(f, "]")
    }
    ast::Pat::VectorPat(pat) => {
      output(f, "#[")?;
      pat_args(f, pat.list_pat()?.pat_args())?;
      output(f, "]")
    }
    ast::Pat::InfixPat(pat) => {
      get_pat(f, pat.lhs()?)?;
      output(f, " ")?;
      output(f, pat.name_star_eq()?.token.text())?;
      output(f, " ")?;
      get_pat(f, pat.rhs()?)
    }
    ast::Pat::TypedPat(pat) => {
      get_pat(f, pat.pat()?)?;
      output(f, " : ")?;
      get_ty(f, pat.ty()?)
    }
    ast::Pat::AsPat(pat) => {
      get_pat(f, pat.pat()?)?;
      output(f, " as ")?;
      get_pat(f, pat.as_pat_tail()?.pat()?)
    }
    ast::Pat::OrPat(pat) => {
      get_pat(f, pat.lhs()?)?;
      output(f, " | ")?;
      get_pat(f, pat.rhs()?)
    }
  }
}

fn get_ty(f: &mut fmt::Formatter<'_>, ty: ast::Ty) -> Res {
  match ty {
    ast::Ty::HoleTy(_) => output(f, "..."),
    ast::Ty::WildcardTy(_) => output(f, "_"),
    ast::Ty::TyVarTy(ty) => output(f, ty.ty_var()?.text()),
    ast::Ty::RecordTy(ty) => {
      output(f, "{")?;
      sep(f, ", ", ty.ty_rows(), |f, row| {
        output(f, row.lab()?.token.text())?;
        output(f, " : ")?;
        get_ty(f, row.ty()?)
      })?;
      output(f, "}")
    }
    ast::Ty::ConTy(ty) => {
      if let Some(ty_seq) = ty.ty_seq() {
        output(f, "(")?;
        sep(f, ", ", ty_seq.ty_args(), |f, t| get_ty(f, t.ty()?))?;
        output(f, ") ")?;
      }
      path(f, ty.path()?)
    }
    ast::Ty::OneArgConTy(ty) => {
      get_ty(f, ty.ty()?)?;
      output(f, " ")?;
      path(f, ty.path()?)
    }
    ast::Ty::TupleTy(ty) => {
      get_ty(f, ty.ty()?);
      for ty in ty.star_tys() {
        output(f, " * ")?;
        get_ty(f, ty.ty()?)?;
      }
      Some(())
    }
    ast::Ty::FnTy(ty) => {
      get_ty(f, ty.param()?)?;
      output(f, " -> ")?;
      get_ty(f, ty.res()?)
    }
    ast::Ty::ParenTy(ty) => {
      output(f, "(")?;
      get_ty(f, ty.ty()?)?;
      output(f, ")")
    }
  }
}

fn in_end<F1, F2>(f: &mut fmt::Formatter<'_>, cfg: Cfg, kw: &str, f1: F1, f2: F2) -> Res
where
  F1: FnOnce(&mut fmt::Formatter<'_>, Cfg) -> Res,
  F2: FnOnce(&mut fmt::Formatter<'_>, Cfg) -> Res,
{
  output(f, kw)?;
  output(f, "\n")?;
  let new_cfg = cfg.indented();
  new_cfg.output_indent(f)?;
  f1(f, new_cfg)?;
  output(f, "\n")?;
  cfg.output_indent(f)?;
  output(f, "in\n")?;
  new_cfg.output_indent(f)?;
  f2(f, new_cfg)?;
  output(f, "\n")?;
  cfg.output_indent(f)?;
  output(f, "end")
}

fn path(f: &mut fmt::Formatter<'_>, p: ast::Path) -> Res {
  sep(f, ".", p.name_star_eq_dots(), |f, x| output(f, x.name_star_eq()?.token.text()))
}

fn pat_args<I>(f: &mut fmt::Formatter<'_>, iter: I) -> Res
where
  I: Iterator<Item = ast::PatArg>,
{
  sep(f, ", ", iter.map(|x| x.pat()), |f, p| get_pat(f, p?))
}

fn exp_args<I>(f: &mut fmt::Formatter<'_>, cfg: Cfg, iter: I) -> Res
where
  I: Iterator<Item = ast::ExpArg>,
{
  sep(f, ", ", iter.map(|x| x.exp()), |f, e| get_exp(f, cfg, e?))
}

fn sep<F, I, T>(f: &mut fmt::Formatter<'_>, s: &str, mut iter: I, mut get_t: F) -> Res
where
  F: FnMut(&mut fmt::Formatter<'_>, T) -> Res,
  I: Iterator<Item = T>,
{
  if let Some(arg) = iter.next() {
    get_t(f, arg)?;
  }
  for arg in iter {
    output(f, s)?;
    get_t(f, arg)?;
  }
  Some(())
}

fn sep_with_lines<F, I, T>(
  f: &mut fmt::Formatter<'_>,
  cfg: Cfg,
  s: &str,
  mut iter: I,
  mut get_t: F,
) -> Res
where
  F: FnMut(&mut fmt::Formatter<'_>, T) -> Res,
  I: Iterator<Item = T>,
{
  if let Some(arg) = iter.next() {
    get_t(f, arg)?;
  }
  for arg in iter {
    output(f, "\n")?;
    cfg.output_indent(f)?;
    output(f, s)?;
    get_t(f, arg)?;
  }
  Some(())
}

fn exp_semi_seq<I>(f: &mut fmt::Formatter<'_>, cfg: Cfg, mut iter: I) -> Res
where
  I: Iterator<Item = ast::ExpInSeq>,
{
  if let Some(e) = iter.next() {
    get_exp(f, cfg, e.exp()?)?;
  }
  for e in iter {
    output(f, ";\n")?;
    cfg.output_indent(f)?;
    get_exp(f, cfg, e.exp()?)?;
  }
  Some(())
}
