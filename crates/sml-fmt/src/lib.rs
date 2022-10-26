//! Format SML files.
//!
//! Very WIP.
//!
//! - Doesn't handle comments.
//! - No attempt is made to restrict lines to a reasonable length.
//!
//! Basically: don't use this.

#![deny(clippy::pedantic, missing_debug_implementations, missing_docs, rust_2018_idioms)]
#![allow(clippy::needless_pass_by_value, clippy::too_many_lines)]

use fast_hash::FxHashSet;
use sml_syntax::ast::{self, AstNode as _};
use sml_syntax::rowan::TextRange;
use sml_syntax::SyntaxKind;

/// Returns either the
///
/// # Errors
///
/// If there was a syntax error or comments in an un-format-able position.
pub fn get(root: &ast::Root) -> Result<String, Error> {
  let mut st = St {
    buf: String::new(),
    comment_ranges: root
      .syntax()
      .descendants_with_tokens()
      .filter_map(|x| {
        let tok = x.into_token()?;
        (tok.kind() == SyntaxKind::BlockComment).then(|| tok.text_range())
      })
      .collect(),
  };
  match root.dec().and_then(|d| get_dec(&mut st, Cfg::default(), d)) {
    Some(()) => {
      if st.comment_ranges.is_empty() {
        Ok(st.buf)
      } else {
        Err(Error::Comments(st.comment_ranges))
      }
    }
    None => Err(Error::Syntax),
  }
}

/// A failure to format a file.
#[derive(Debug)]
pub enum Error {
  /// There is a syntax error in the file that prevents formatting.
  Syntax,
  /// There were comments that couldn't be formatted.
  Comments(FxHashSet<TextRange>),
}

#[derive(Debug, Default)]
struct St {
  buf: String,
  comment_ranges: FxHashSet<TextRange>,
}

type Res = Option<()>;

#[derive(Debug, Clone, Copy)]
struct Cfg {
  indent: usize,
  extra_blank: bool,
}

impl Default for Cfg {
  fn default() -> Self {
    Self { indent: 0, extra_blank: true }
  }
}

impl Cfg {
  fn indented(self) -> Self {
    Self { indent: self.indent + 1, ..self }
  }

  fn extra_blank(self, extra_blank: bool) -> Self {
    Self { extra_blank, ..self }
  }

  fn output_indent(&self, st: &mut St) {
    for _ in 0..self.indent {
      st.buf.push_str("  ");
    }
  }
}

fn get_dec(st: &mut St, cfg: Cfg, dec: ast::Dec) -> Res {
  sep_with_lines(st, cfg, "", dec.dec_with_tail_in_seqs(), |st, dwt_in_seq| {
    let dwt = dwt_in_seq.dec_with_tail()?;
    sep_with_lines(st, cfg, "", dwt.dec_in_seqs(), |st, dec_in_seq| {
      get_dec_one(st, cfg.extra_blank(false), dec_in_seq.dec_one()?)?;
      if dec_in_seq.semicolon().is_some() {
        st.buf.push(';');
      }
      Some(())
    })?;
    sep_with_lines(st, cfg, "", dwt.sharing_tails(), |st, sharing| {
      st.buf.push_str("sharing type ");
      sep(st, " = ", sharing.path_eqs(), |st, p| path(st, p.path()?))
    })?;
    if dwt_in_seq.semicolon().is_some() {
      st.buf.push(';');
    }
    Some(())
  })
}

fn get_dec_one(st: &mut St, cfg: Cfg, dec: ast::DecOne) -> Res {
  if let Some(tok) = sml_comment::comment_above(dec.syntax()) {
    st.comment_ranges.remove(&tok.text_range());
    cfg.output_indent(st);
    st.buf.push_str(tok.text().trim());
    st.buf.push('\n');
  }
  match dec {
    ast::DecOne::HoleDec(_) => st.buf.push_str("..."),
    ast::DecOne::ValDec(dec) => {
      st.buf.push_str("val ");
      sep_with_lines(st, cfg, "and ", dec.val_binds(), |st, val_bind| {
        get_pat(st, val_bind.pat()?)?;
        if let Some(eq_exp) = val_bind.eq_exp() {
          st.buf.push_str(" = ");
          get_exp(st, cfg, eq_exp.exp()?)?;
        }
        Some(())
      })?;
    }
    ast::DecOne::FunDec(dec) => {
      st.buf.push_str("fun ");
      ty_var_seq(st, dec.ty_var_seq())?;
      sep_with_lines(st, cfg, "and ", dec.fun_binds(), |st, fun_bind| {
        sep_with_lines(st, cfg.indented(), "| ", fun_bind.fun_bind_cases(), |st, fun_bind_case| {
          match fun_bind_case.fun_bind_case_head()? {
            ast::FunBindCaseHead::PrefixFunBindCaseHead(head) => {
              if head.op_kw().is_some() {
                st.buf.push_str("op ");
              }
              st.buf.push_str(head.name_star_eq()?.token.text());
            }
            ast::FunBindCaseHead::InfixFunBindCaseHead(head) => {
              let parens = head.l_round().is_some();
              if parens {
                st.buf.push('(');
              }
              get_pat(st, head.lhs()?)?;
              st.buf.push(' ');
              st.buf.push_str(head.name_star_eq()?.token.text());
              st.buf.push(' ');
              get_pat(st, head.lhs()?)?;
              if parens {
                st.buf.push(')');
              }
            }
          }
          st.buf.push(' ');
          sep(st, " ", fun_bind_case.pats(), get_pat)?;
          ty_annotation(st, fun_bind_case.ty_annotation())?;
          if let Some(eq_exp) = fun_bind_case.eq_exp() {
            st.buf.push_str(" =");
            get_body_exp(st, cfg, eq_exp.exp()?)?;
          }
          Some(())
        })
      })?;
    }
    ast::DecOne::TyDec(dec) => {
      st.buf.push_str("type ");
      ty_binds(st, cfg, dec.ty_binds())?;
    }
    ast::DecOne::DatDec(dec) => {
      st.buf.push_str("datatype ");
      dat_binds(st, cfg, dec.dat_binds())?;
      if let Some(withtype) = dec.with_type() {
        st.buf.push_str(" withtype ");
        ty_binds(st, cfg, withtype.ty_binds())?;
      }
    }
    ast::DecOne::DatCopyDec(dec) => {
      st.buf.push_str("datatype ");
      st.buf.push_str(dec.name()?.text());
      st.buf.push_str(" = datatype ");
      path(st, dec.path()?)?;
    }
    ast::DecOne::AbstypeDec(dec) => {
      st.buf.push_str("abstype ");
      dat_binds(st, cfg, dec.dat_binds())?;
      if let Some(withtype) = dec.with_type() {
        st.buf.push_str(" withtype ");
        ty_binds(st, cfg, withtype.ty_binds())?;
      }
      st.buf.push_str(" with ");
      get_dec(st, cfg, dec.dec()?)?;
      st.buf.push_str(" end");
    }
    ast::DecOne::ExDec(dec) => {
      st.buf.push_str("exception ");
      sep_with_lines(st, cfg, "and ", dec.ex_binds(), |st, ex_bind| {
        st.buf.push_str(ex_bind.name_star_eq()?.token.text());
        match ex_bind.ex_bind_inner() {
          Some(inner) => match inner {
            ast::ExBindInner::OfTy(of_ty) => {
              st.buf.push_str(" of ");
              get_ty(st, of_ty.ty()?)
            }
            ast::ExBindInner::EqPath(eq_path) => {
              st.buf.push_str(" = ");
              path(st, eq_path.path()?)
            }
          },
          None => Some(()),
        }
      })?;
    }
    ast::DecOne::OpenDec(dec) => {
      st.buf.push_str("open ");
      sep(st, " ", dec.paths(), path)?;
    }
    ast::DecOne::InfixDec(dec) => {
      st.buf.push_str("infix ");
      if let Some(int_lit) = dec.int_lit() {
        st.buf.push_str(int_lit.text());
        st.buf.push(' ');
      }
      names_space(st, dec.name_star_eqs())?;
    }
    ast::DecOne::InfixrDec(dec) => {
      st.buf.push_str("infixr ");
      if let Some(int_lit) = dec.int_lit() {
        st.buf.push_str(int_lit.text());
        st.buf.push(' ');
      }
      names_space(st, dec.name_star_eqs())?;
    }
    ast::DecOne::NonfixDec(dec) => {
      st.buf.push_str("nonfix ");
      names_space(st, dec.name_star_eqs())?;
    }
    ast::DecOne::DoDec(dec) => {
      st.buf.push_str("do ");
      get_exp(st, cfg, dec.exp()?)?;
    }
    ast::DecOne::LocalDec(dec) => in_end(
      st,
      cfg,
      "local",
      |st, cfg| get_dec(st, cfg, dec.local_dec()?),
      |st, cfg| get_dec(st, cfg, dec.in_dec()?),
    )?,
    ast::DecOne::StructureDec(dec) => {
      st.buf.push_str("structure ");
      sep_with_lines(st, cfg, "and ", dec.str_binds(), |st, str_bind| {
        st.buf.push_str(str_bind.name()?.text());
        if let Some(tail) = str_bind.ascription_tail() {
          ascription_tail(st, cfg, tail)?;
        }
        if let Some(eq_str_exp) = str_bind.eq_str_exp() {
          st.buf.push_str(" = ");
          get_str_exp(st, cfg, eq_str_exp.str_exp()?)?;
        }
        Some(())
      })?;
    }
    ast::DecOne::SignatureDec(dec) => {
      st.buf.push_str("signature ");
      sep_with_lines(st, cfg, "and ", dec.sig_binds(), |st, sig_bind| {
        st.buf.push_str(sig_bind.name()?.text());
        st.buf.push_str(" = ");
        get_sig_exp(st, cfg, sig_bind.sig_exp()?)
      })?;
    }
    ast::DecOne::FunctorDec(dec) => {
      st.buf.push_str("functor ");
      sep_with_lines(st, cfg, "and ", dec.functor_binds(), |st, functor_bind| {
        st.buf.push_str(functor_bind.functor_name()?.text());
        st.buf.push_str(" (");
        match functor_bind.functor_arg()? {
          ast::FunctorArg::FunctorArgNameSigExp(arg) => {
            st.buf.push_str(arg.name()?.text());
            st.buf.push_str(" : ");
            get_sig_exp(st, cfg, arg.sig_exp()?)?;
          }
          ast::FunctorArg::Dec(dec) => {
            st.buf.push('\n');
            let new_cfg = cfg.indented();
            new_cfg.output_indent(st);
            get_dec(st, new_cfg, dec)?;
            st.buf.push('\n');
          }
        }
        st.buf.push(')');
        if let Some(tail) = functor_bind.ascription_tail() {
          ascription_tail(st, cfg, tail)?;
        }
        st.buf.push_str(" = ");
        get_str_exp(st, cfg, functor_bind.body()?)
      })?;
    }
    ast::DecOne::ExpDec(dec) => get_exp(st, cfg, dec.exp()?)?,
    ast::DecOne::IncludeDec(dec) => {
      st.buf.push_str("include ");
      sep(st, " ", dec.sig_exps(), |st, sig_exp| get_sig_exp(st, cfg, sig_exp))?;
    }
  }
  Some(())
}

fn names_space<I>(st: &mut St, iter: I) -> Res
where
  I: Iterator<Item = ast::NameStarEq>,
{
  sep(st, " ", iter, |st, n| {
    st.buf.push_str(n.token.text());
    Some(())
  })
}

fn dat_binds<I>(st: &mut St, cfg: Cfg, iter: I) -> Res
where
  I: Iterator<Item = ast::DatBind>,
{
  sep_with_lines(st, cfg, "and ", iter, |st, dat_bind| {
    ty_var_seq(st, dat_bind.ty_var_seq())?;
    st.buf.push_str(dat_bind.name()?.text());
    let eq_con_binds = dat_bind.eq_con_binds()?;
    if eq_con_binds.con_binds().count() > 1 {
      st.buf.push_str(" =\n");
      cfg.indented().output_indent(st);
      sep_with_lines(st, cfg, "| ", eq_con_binds.con_binds(), con_bind)
    } else {
      st.buf.push_str(" = ");
      sep(st, " | ", eq_con_binds.con_binds(), con_bind)
    }
  })
}

fn con_bind(st: &mut St, con_bind: ast::ConBind) -> Res {
  st.buf.push_str(con_bind.name_star_eq()?.token.text());
  if let Some(of_ty) = con_bind.of_ty() {
    st.buf.push_str(" of ");
    get_ty(st, of_ty.ty()?)?;
  }
  Some(())
}

fn ascription_tail(st: &mut St, cfg: Cfg, tail: ast::AscriptionTail) -> Res {
  st.buf.push(' ');
  st.buf.push_str(tail.ascription()?.token.text());
  st.buf.push(' ');
  get_sig_exp(st, cfg, tail.sig_exp()?)
}

fn get_str_exp(st: &mut St, cfg: Cfg, str_exp: ast::StrExp) -> Res {
  match str_exp {
    ast::StrExp::StructStrExp(exp) => {
      st.buf.push_str("struct\n");
      let new_cfg = cfg.indented().extra_blank(true);
      new_cfg.output_indent(st);
      get_dec(st, new_cfg, exp.dec()?)?;
      st.buf.push('\n');
      cfg.output_indent(st);
      st.buf.push_str("end");
    }
    ast::StrExp::PathStrExp(exp) => path(st, exp.path()?)?,
    ast::StrExp::AscriptionStrExp(exp) => {
      get_str_exp(st, cfg, exp.str_exp()?)?;
      ascription_tail(st, cfg, exp.ascription_tail()?)?;
    }
    ast::StrExp::AppStrExp(exp) => {
      st.buf.push_str(exp.name()?.text());
      st.buf.push_str(" (");
      match exp.app_str_exp_arg()? {
        ast::AppStrExpArg::AppStrExpArgStrExp(arg) => get_str_exp(st, cfg, arg.str_exp()?)?,
        ast::AppStrExpArg::Dec(arg) => get_dec(st, cfg, arg)?,
      }
      st.buf.push(')');
    }
    ast::StrExp::LetStrExp(exp) => in_end(
      st,
      cfg,
      "let",
      |st, cfg| get_str_exp(st, cfg, exp.str_exp()?),
      |st, cfg| get_dec(st, cfg, exp.dec()?),
    )?,
  }
  Some(())
}

fn get_sig_exp(st: &mut St, cfg: Cfg, sig_exp: ast::SigExp) -> Res {
  match sig_exp {
    ast::SigExp::SigSigExp(exp) => {
      st.buf.push_str("sig\n");
      let new_cfg = cfg.indented();
      new_cfg.output_indent(st);
      get_dec(st, new_cfg, exp.dec()?)?;
      st.buf.push('\n');
      cfg.output_indent(st);
      st.buf.push_str("end");
    }
    ast::SigExp::NameSigExp(exp) => st.buf.push_str(exp.name()?.text()),
    ast::SigExp::WhereTypeSigExp(exp) => {
      get_sig_exp(st, cfg, exp.sig_exp()?)?;
      st.buf.push_str(" where type ");
      ty_var_seq(st, exp.ty_var_seq())?;
      path(st, exp.path()?)?;
      st.buf.push_str(" = ");
      get_ty(st, exp.ty()?)?;
    }
    ast::SigExp::WhereSigExp(exp) => {
      get_sig_exp(st, cfg, exp.sig_exp()?)?;
      st.buf.push_str(" where ");
      path(st, exp.lhs()?)?;
      st.buf.push_str(" = ");
      path(st, exp.rhs()?)?;
    }
  }
  Some(())
}

fn ty_annotation(st: &mut St, ty_ann: Option<ast::TyAnnotation>) -> Res {
  match ty_ann {
    Some(ty_ann) => {
      st.buf.push_str(" : ");
      get_ty(st, ty_ann.ty()?)
    }
    None => Some(()),
  }
}

fn ty_binds<I>(st: &mut St, cfg: Cfg, iter: I) -> Res
where
  I: Iterator<Item = ast::TyBind>,
{
  sep_with_lines(st, cfg, "and ", iter, |st, ty_bind| {
    ty_var_seq(st, ty_bind.ty_var_seq())?;
    st.buf.push_str(ty_bind.name()?.text());
    if let Some(eq_ty) = ty_bind.eq_ty() {
      st.buf.push_str(" = ");
      get_ty(st, eq_ty.ty()?)?;
    }
    Some(())
  })
}

/// if there was a ty var seq, this'll also add a space after it.
fn ty_var_seq(st: &mut St, tvs: Option<ast::TyVarSeq>) -> Res {
  let tvs = match tvs {
    Some(x) => x,
    None => return Some(()),
  };
  let parens = tvs.l_round().is_some();
  if parens {
    st.buf.push('(');
  }
  sep(st, ", ", tvs.ty_var_args(), |st, tv| {
    st.buf.push_str(tv.ty_var()?.text());
    Some(())
  })?;
  if parens {
    st.buf.push(')');
  }
  st.buf.push(' ');
  Some(())
}

fn get_body_exp(st: &mut St, cfg: Cfg, exp: ast::Exp) -> Res {
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
    st.buf.push('\n');
    let new_cfg = cfg.indented();
    new_cfg.output_indent(st);
    get_exp(st, new_cfg, exp)
  } else {
    st.buf.push(' ');
    get_exp(st, cfg, exp)
  }
}

fn get_exp(st: &mut St, cfg: Cfg, exp: ast::Exp) -> Res {
  match exp {
    ast::Exp::HoleExp(_) => st.buf.push_str("..."),
    ast::Exp::WildcardExp(_) => st.buf.push('_'),
    ast::Exp::OpAndalsoExp(_) => st.buf.push_str("op andalso"),
    ast::Exp::OpOrelseExp(_) => st.buf.push_str("op orelse"),
    ast::Exp::SConExp(exp) => st.buf.push_str(exp.s_con()?.token.text()),
    ast::Exp::PathExp(exp) => {
      if exp.op_kw().is_some() {
        st.buf.push_str("op ");
      }
      path(st, exp.path()?)?;
    }
    ast::Exp::RecordExp(exp) => {
      st.buf.push('{');
      sep(st, ", ", exp.exp_rows(), |st, row| {
        st.buf.push_str(row.lab()?.token.text());
        if let Some(eq_exp) = row.eq_exp() {
          st.buf.push_str(" = ");
          get_exp(st, cfg, eq_exp.exp()?)?;
        }
        Some(())
      })?;
      st.buf.push('}');
    }
    ast::Exp::SelectorExp(exp) => {
      st.buf.push('#');
      st.buf.push_str(exp.lab()?.token.text());
    }
    ast::Exp::ParenExp(exp) => {
      st.buf.push('(');
      get_exp(st, cfg, exp.exp()?)?;
      st.buf.push(')');
    }
    ast::Exp::TupleExp(exp) => {
      st.buf.push('(');
      exp_args(st, cfg, exp.exp_args())?;
      st.buf.push(')');
    }
    ast::Exp::ListExp(exp) => {
      st.buf.push('[');
      exp_args(st, cfg, exp.exp_args())?;
      st.buf.push(']');
    }
    ast::Exp::VectorExp(exp) => {
      st.buf.push_str("#[");
      exp_args(st, cfg, exp.list_exp()?.exp_args())?;
      st.buf.push(']');
    }
    ast::Exp::SeqExp(exp) => {
      st.buf.push_str("(\n");
      let new_cfg = cfg.indented();
      new_cfg.output_indent(st);
      exp_semi_seq(st, new_cfg, exp.exps_in_seq())?;
      st.buf.push('\n');
      cfg.output_indent(st);
      st.buf.push(')');
    }
    ast::Exp::LetExp(exp) => in_end(
      st,
      cfg,
      "let",
      |st, cfg| get_dec(st, cfg, exp.dec()?),
      |st, cfg| exp_semi_seq(st, cfg, exp.exps_in_seq()),
    )?,
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
      get_exp(st, cfg, func)?;
      if !func_is_bang || arg_is_symbolic {
        st.buf.push(' ');
      }
      get_exp(st, cfg, arg)?;
    }
    ast::Exp::InfixExp(exp) => {
      get_exp(st, cfg, exp.lhs()?)?;
      st.buf.push(' ');
      st.buf.push_str(exp.name_star_eq()?.token.text());
      st.buf.push(' ');
      get_exp(st, cfg, exp.rhs()?)?;
    }
    ast::Exp::TypedExp(exp) => {
      get_exp(st, cfg, exp.exp()?)?;
      st.buf.push_str(" : ");
      get_ty(st, exp.ty()?)?;
    }
    ast::Exp::AndalsoExp(exp) => {
      get_exp(st, cfg, exp.lhs()?)?;
      st.buf.push_str(" andalso ");
      get_exp(st, cfg, exp.rhs()?)?;
    }
    ast::Exp::OrelseExp(exp) => {
      get_exp(st, cfg, exp.lhs()?)?;
      st.buf.push_str(" orelse ");
      get_exp(st, cfg, exp.rhs()?)?;
    }
    ast::Exp::HandleExp(exp) => {
      get_exp(st, cfg, exp.exp()?)?;
      st.buf.push_str(" handle\n");
      get_matcher(st, cfg, exp.matcher()?)?;
    }
    ast::Exp::RaiseExp(exp) => {
      st.buf.push_str("raise ");
      get_exp(st, cfg, exp.exp()?)?;
    }
    ast::Exp::IfExp(exp) => {
      st.buf.push_str("if ");
      get_exp(st, cfg, exp.cond()?)?;
      st.buf.push_str(" then\n");
      let new_cfg = cfg.indented();
      new_cfg.output_indent(st);
      get_exp(st, new_cfg, exp.yes()?)?;
      st.buf.push('\n');
      cfg.output_indent(st);
      st.buf.push_str("else");
      let no = exp.no()?;
      if matches!(no, ast::Exp::IfExp(_)) {
        st.buf.push(' ');
        get_exp(st, cfg, no)?;
      } else {
        st.buf.push('\n');
        new_cfg.output_indent(st);
        get_exp(st, new_cfg, no)?;
      }
    }
    ast::Exp::WhileExp(exp) => {
      st.buf.push_str("while");
      get_exp(st, cfg, exp.cond()?)?;
      st.buf.push_str(" do ");
      get_exp(st, cfg, exp.body()?)?;
    }
    ast::Exp::CaseExp(exp) => {
      st.buf.push_str("case ");
      get_exp(st, cfg, exp.exp()?)?;
      st.buf.push_str(" of\n");
      get_matcher(st, cfg, exp.matcher()?)?;
    }
    ast::Exp::FnExp(exp) => {
      st.buf.push_str("fn ");
      get_matcher(st, cfg, exp.matcher()?)?;
    }
  }
  Some(())
}

fn get_matcher(st: &mut St, cfg: Cfg, matcher: ast::Matcher) -> Res {
  cfg.indented().output_indent(st);
  sep_with_lines(st, cfg, "| ", matcher.match_rules(), |st, arm| {
    get_pat(st, arm.pat()?)?;
    st.buf.push_str(" =>");
    get_body_exp(st, cfg, arm.exp()?)
  })
}

fn get_pat(st: &mut St, pat: ast::Pat) -> Res {
  match pat {
    ast::Pat::WildcardPat(_) => st.buf.push('_'),
    ast::Pat::SConPat(pat) => st.buf.push_str(pat.s_con()?.token.text()),
    ast::Pat::ConPat(pat) => {
      if pat.op_kw().is_some() {
        st.buf.push_str("op ");
      }
      path(st, pat.path()?)?;
      if let Some(pat) = pat.pat() {
        st.buf.push(' ');
        get_pat(st, pat)?;
      }
    }
    ast::Pat::RecordPat(pat) => {
      st.buf.push('{');
      sep(st, ", ", pat.pat_rows(), |st, row| {
        match row.pat_row_inner()? {
          ast::PatRowInner::RestPatRow(_) => st.buf.push_str("..."),
          ast::PatRowInner::LabAndPatPatRow(row) => {
            st.buf.push_str(row.lab()?.token.text());
            st.buf.push_str(" = ");
            get_pat(st, row.pat()?)?;
          }
          ast::PatRowInner::LabPatRow(row) => {
            st.buf.push_str(row.name_star_eq()?.token.text());
            ty_annotation(st, row.ty_annotation())?;
            if let Some(tail) = row.as_pat_tail() {
              st.buf.push_str(" as ");
              get_pat(st, tail.pat()?)?;
            }
          }
        }
        Some(())
      })?;
      st.buf.push('}');
    }
    ast::Pat::ParenPat(pat) => {
      st.buf.push('(');
      get_pat(st, pat.pat()?)?;
      st.buf.push(')');
    }
    ast::Pat::TuplePat(pat) => {
      st.buf.push('(');
      pat_args(st, pat.pat_args())?;
      st.buf.push(')');
    }
    ast::Pat::ListPat(pat) => {
      st.buf.push('[');
      pat_args(st, pat.pat_args())?;
      st.buf.push(']');
    }
    ast::Pat::VectorPat(pat) => {
      st.buf.push_str("#[");
      pat_args(st, pat.list_pat()?.pat_args())?;
      st.buf.push(']');
    }
    ast::Pat::InfixPat(pat) => {
      get_pat(st, pat.lhs()?)?;
      st.buf.push(' ');
      st.buf.push_str(pat.name_star_eq()?.token.text());
      st.buf.push(' ');
      get_pat(st, pat.rhs()?)?;
    }
    ast::Pat::TypedPat(pat) => {
      get_pat(st, pat.pat()?)?;
      st.buf.push_str(" : ");
      get_ty(st, pat.ty()?)?;
    }
    ast::Pat::AsPat(pat) => {
      get_pat(st, pat.pat()?)?;
      st.buf.push_str(" as ");
      get_pat(st, pat.as_pat_tail()?.pat()?)?;
    }
    ast::Pat::OrPat(pat) => {
      get_pat(st, pat.lhs()?)?;
      st.buf.push_str(" | ");
      get_pat(st, pat.rhs()?)?;
    }
  }
  Some(())
}

fn get_ty(st: &mut St, ty: ast::Ty) -> Res {
  match ty {
    ast::Ty::HoleTy(_) => st.buf.push_str("..."),
    ast::Ty::WildcardTy(_) => st.buf.push('_'),
    ast::Ty::TyVarTy(ty) => st.buf.push_str(ty.ty_var()?.text()),
    ast::Ty::RecordTy(ty) => {
      st.buf.push('{');
      sep(st, ", ", ty.ty_rows(), |st, row| {
        st.buf.push_str(row.lab()?.token.text());
        st.buf.push_str(" : ");
        get_ty(st, row.ty()?)
      })?;
      st.buf.push('}');
    }
    ast::Ty::ConTy(ty) => {
      if let Some(ty_seq) = ty.ty_seq() {
        st.buf.push('(');
        sep(st, ", ", ty_seq.ty_args(), |st, t| get_ty(st, t.ty()?))?;
        st.buf.push_str(") ");
      }
      path(st, ty.path()?)?;
    }
    ast::Ty::OneArgConTy(ty) => {
      get_ty(st, ty.ty()?)?;
      st.buf.push(' ');
      path(st, ty.path()?)?;
    }
    ast::Ty::TupleTy(ty) => {
      get_ty(st, ty.ty()?);
      for ty in ty.star_tys() {
        st.buf.push_str(" * ");
        get_ty(st, ty.ty()?)?;
      }
    }
    ast::Ty::FnTy(ty) => {
      get_ty(st, ty.param()?)?;
      st.buf.push_str(" -> ");
      get_ty(st, ty.res()?)?;
    }
    ast::Ty::ParenTy(ty) => {
      st.buf.push('(');
      get_ty(st, ty.ty()?)?;
      st.buf.push(')');
    }
  }
  Some(())
}

fn in_end<F1, F2>(st: &mut St, cfg: Cfg, kw: &str, f1: F1, f2: F2) -> Res
where
  F1: FnOnce(&mut St, Cfg) -> Res,
  F2: FnOnce(&mut St, Cfg) -> Res,
{
  st.buf.push_str(kw);
  st.buf.push('\n');
  let new_cfg = cfg.indented();
  new_cfg.output_indent(st);
  f1(st, new_cfg)?;
  st.buf.push('\n');
  cfg.output_indent(st);
  st.buf.push_str("in\n");
  new_cfg.output_indent(st);
  f2(st, new_cfg)?;
  st.buf.push('\n');
  cfg.output_indent(st);
  st.buf.push_str("end");
  Some(())
}

fn path(st: &mut St, p: ast::Path) -> Res {
  sep(st, ".", p.name_star_eq_dots(), |st, x| {
    st.buf.push_str(x.name_star_eq()?.token.text());
    Some(())
  })
}

fn pat_args<I>(st: &mut St, iter: I) -> Res
where
  I: Iterator<Item = ast::PatArg>,
{
  sep(st, ", ", iter.map(|x| x.pat()), |st, p| get_pat(st, p?))
}

fn exp_args<I>(st: &mut St, cfg: Cfg, iter: I) -> Res
where
  I: Iterator<Item = ast::ExpArg>,
{
  sep(st, ", ", iter.map(|x| x.exp()), |st, e| get_exp(st, cfg, e?))
}

fn sep<F, I, T>(st: &mut St, s: &str, mut iter: I, mut get_t: F) -> Res
where
  F: FnMut(&mut St, T) -> Res,
  I: Iterator<Item = T>,
{
  if let Some(arg) = iter.next() {
    get_t(st, arg)?;
  }
  for arg in iter {
    st.buf.push_str(s);
    get_t(st, arg)?;
  }
  Some(())
}

fn sep_with_lines<F, I, T>(st: &mut St, cfg: Cfg, s: &str, mut iter: I, mut get_t: F) -> Res
where
  F: FnMut(&mut St, T) -> Res,
  I: Iterator<Item = T>,
{
  if let Some(arg) = iter.next() {
    get_t(st, arg)?;
  }
  for arg in iter {
    st.buf.push('\n');
    if cfg.extra_blank {
      st.buf.push('\n');
    }
    cfg.output_indent(st);
    st.buf.push_str(s);
    get_t(st, arg)?;
  }
  Some(())
}

fn exp_semi_seq<I>(st: &mut St, cfg: Cfg, mut iter: I) -> Res
where
  I: Iterator<Item = ast::ExpInSeq>,
{
  if let Some(e) = iter.next() {
    get_exp(st, cfg, e.exp()?)?;
  }
  for e in iter {
    st.buf.push_str(";\n");
    cfg.output_indent(st);
    get_exp(st, cfg, e.exp()?)?;
  }
  Some(())
}
