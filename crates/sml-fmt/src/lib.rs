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
  sep(f, "\n\n", dec.dec_in_seqs(), |f, dec_in_seq| {
    cfg.output_indent(f)?;
    get_dec_one(f, cfg, dec_in_seq.dec_one()?)
  })
}

/// TODO rm
fn nothing() -> Res {
  Some(())
}

/// TODO be more careful with `;`?
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
          let body = fun_bind_case.exp()?;
          if complex(&body) {
            output(f, "\n");
            let new_cfg = cfg.indented();
            new_cfg.output_indent(f)?;
            get_exp(f, new_cfg, body)
          } else {
            output(f, " ")?;
            get_exp(f, cfg, body)
          }
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
    ast::DecOne::LocalDec(dec) => {
      output(f, "local\n")?;
      let new_cfg = cfg.indented();
      get_dec(f, new_cfg, dec.local_dec()?)?;
      output(f, "\n")?;
      cfg.output_indent(f)?;
      output(f, "in\n")?;
      get_dec(f, new_cfg, dec.in_dec()?)?;
      output(f, "\n")?;
      cfg.output_indent(f)?;
      output(f, "end")
    }
    ast::DecOne::StructureDec(_) => nothing(),
    ast::DecOne::SignatureDec(_) => nothing(),
    ast::DecOne::FunctorDec(_) => nothing(),
    ast::DecOne::ExpDec(dec) => get_exp(f, cfg, dec.exp()?),
  }
}

fn complex(exp: &ast::Exp) -> bool {
  matches!(
    exp,
    ast::Exp::LetExp(_)
      | ast::Exp::HandleExp(_)
      | ast::Exp::IfExp(_)
      | ast::Exp::WhileExp(_)
      | ast::Exp::CaseExp(_)
  )
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
      output(f, "(")?;
      sep(f, "; ", exp.exps_in_seq().map(|x| x.exp()), |f, e| get_exp(f, cfg, e?))?;
      output(f, ")")
    }
    ast::Exp::LetExp(exp) => {
      output(f, "let\n")?;
      let new_cfg = cfg.indented();
      get_dec(f, new_cfg, exp.dec()?)?;
      output(f, "\n")?;
      cfg.output_indent(f)?;
      output(f, "in\n")?;
      new_cfg.output_indent(f)?;
      sep(f, "; ", exp.exps_in_seq().map(|x| x.exp()), |f, e| get_exp(f, cfg, e?))?;
      output(f, "\n")?;
      cfg.output_indent(f)?;
      output(f, "end")
    }
    ast::Exp::AppExp(exp) => {
      get_exp(f, cfg, exp.func()?)?;
      output(f, " ")?;
      get_exp(f, cfg, exp.arg()?)
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
      output(f, " handle ")?;
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
    output(f, " => ")?;
    get_exp(f, cfg, arm.exp()?)
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
    ast::Pat::RecordPat(_) => nothing(),
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
