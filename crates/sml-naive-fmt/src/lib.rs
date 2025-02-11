//! Formatting SML files.
//!
//! - Only handles certain comments.
//! - No attempt is made to restrict lines to a reasonable length.

#![allow(clippy::needless_pass_by_value, clippy::too_many_lines)]

use fast_hash::FxHashSet;
use sml_syntax::ast::{self, AstNode as _};
use sml_syntax::kind::SyntaxKind;
use text_size_util::TextRange;

/// Returns the formatted syntax tree.
///
/// # Errors
///
/// If there was a syntax error or comments in an un-format-able position.
pub fn get(root: &ast::Root, tab_size: u32) -> Result<String, Error> {
  match go(Mode::Write(String::new()), root, tab_size)? {
    Mode::Write(s) => Ok(s),
    Mode::Check => unreachable!("changed mode to Check"),
  }
}

/// Returns `Ok(())` if the syntax tree could be formatted.
///
/// # Errors
///
/// If there was a syntax error or comments in an un-format-able position.
pub fn check(root: &ast::Root) -> Result<(), Error> {
  match go(Mode::Check, root, 0)? {
    Mode::Write(_) => unreachable!("changed mode to Write"),
    Mode::Check => Ok(()),
  }
}

fn go(mode: Mode, root: &ast::Root, tab_size: u32) -> Result<Mode, Error> {
  let mut st = St {
    mode,
    comment_ranges: root
      .syntax()
      .descendants_with_tokens()
      .filter_map(|x| {
        let tok = x.into_token()?;
        (tok.kind() == SyntaxKind::BlockComment).then(|| tok.text_range())
      })
      .collect(),
  };
  match get_dec(&mut st, Cfg::new(tab_size), root.decs()) {
    Some(()) => {
      if st.comment_ranges.is_empty() {
        Ok(st.mode)
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

#[derive(Debug)]
enum Mode {
  Write(String),
  Check,
}

#[derive(Debug)]
struct St {
  mode: Mode,
  comment_ranges: FxHashSet<TextRange>,
}

impl St {
  fn write(&mut self, s: &str) {
    match &mut self.mode {
      Mode::Write(buf) => buf.push_str(s),
      Mode::Check => {}
    }
  }
}

type Res = Option<()>;

#[derive(Debug, Clone, Copy)]
struct Cfg {
  indent: u32,
  tab_size: u32,
  extra_blank: bool,
}

impl Cfg {
  fn new(tab_size: u32) -> Self {
    Self { indent: 0, tab_size, extra_blank: true }
  }

  fn indented(self) -> Self {
    Self { indent: self.indent + self.tab_size, ..self }
  }

  fn extra_blank(self, extra_blank: bool) -> Self {
    Self { extra_blank, ..self }
  }

  fn output_indent(&self, st: &mut St) {
    for _ in 0..self.indent {
      st.write(" ");
    }
  }
}

fn get_dec<I>(st: &mut St, cfg: Cfg, iter: I) -> Res
where
  I: Iterator<Item = ast::Dec>,
{
  sep_with_lines(st, cfg, "", iter, |st, dwt_in_seq| {
    let dwt = dwt_in_seq.dec_with_tail()?;
    sep_with_lines(st, cfg, "", dwt.dec_in_seqs(), |st, dec_in_seq| {
      get_dec_one(st, cfg.extra_blank(false), dec_in_seq.dec_one()?)?;
      if dec_in_seq.semicolon().is_some() {
        st.write(";");
      }
      Some(())
    })?;
    for sharing in dwt.sharing_tails() {
      st.write("\n");
      cfg.output_indent(st);
      st.write("sharing type ");
      sep(st, " = ", sharing.path_eqs(), |st, p| path(st, p.path()?))?;
    }
    if dwt_in_seq.semicolon().is_some() {
      st.write(";");
    }
    Some(())
  })
}

fn get_dec_one(st: &mut St, cfg: Cfg, dec: ast::DecOne) -> Res {
  if let Some(tok) = sml_comment::comment_above(dec.syntax()) {
    st.comment_ranges.remove(&tok.text_range());
    st.write(tok.text().trim());
    st.write("\n");
    cfg.output_indent(st);
  }
  match dec {
    ast::DecOne::HoleDec(_) => st.write("..."),
    ast::DecOne::ValDec(dec) => {
      st.write("val ");
      sep_with_lines(st, cfg, "and ", dec.val_binds(), |st, val_bind| {
        if val_bind.rec_kw().is_some() {
          st.write("rec ");
        }
        get_pat(st, val_bind.pat()?)?;
        if let Some(eq_exp) = val_bind.eq_exp() {
          st.write(" =");
          get_body_exp(st, cfg, eq_exp.exp()?)?;
        }
        Some(())
      })?;
    }
    ast::DecOne::FunDec(dec) => {
      st.write("fun ");
      ty_var_seq(st, dec.ty_var_seq())?;
      sep_with_lines(st, cfg, "and ", dec.fun_binds(), |st, fun_bind| {
        sep_with_lines(st, cfg.indented(), "| ", fun_bind.fun_bind_cases(), |st, fun_bind_case| {
          match fun_bind_case.fun_bind_case_head()? {
            ast::FunBindCaseHead::PrefixFunBindCaseHead(head) => {
              if head.op_kw().is_some() {
                st.write("op ");
              }
              st.write(head.name_star_eq()?.token.text());
            }
            ast::FunBindCaseHead::InfixFunBindCaseHead(head) => {
              let parens = head.l_round().is_some();
              if parens {
                st.write("(");
              }
              get_pat(st, head.lhs()?)?;
              st.write(" ");
              st.write(head.name_star_eq()?.token.text());
              st.write(" ");
              get_pat(st, head.rhs()?)?;
              if parens {
                st.write(")");
              }
            }
          }
          for arg in fun_bind_case.pats() {
            st.write(" ");
            get_pat(st, arg)?;
          }
          ty_annotation(st, fun_bind_case.ty_annotation())?;
          if let Some(eq_exp) = fun_bind_case.eq_exp() {
            st.write(" =");
            get_body_exp(st, cfg, eq_exp.exp()?)?;
          }
          Some(())
        })
      })?;
    }
    ast::DecOne::TyDec(dec) => {
      st.write(dec.ty_head()?.token.text());
      st.write(" ");
      ty_binds(st, cfg, dec.ty_binds())?;
    }
    ast::DecOne::DatDec(dec) => {
      st.write("datatype ");
      dat_binds(st, cfg, dec.dat_binds())?;
      if let Some(withtype) = dec.with_type() {
        st.write("\n");
        cfg.output_indent(st);
        st.write("withtype ");
        ty_binds(st, cfg, withtype.ty_binds())?;
      }
    }
    ast::DecOne::DatCopyDec(dec) => {
      st.write("datatype ");
      st.write(dec.name()?.text());
      st.write(" = datatype ");
      path(st, dec.path()?)?;
    }
    ast::DecOne::AbstypeDec(dec) => {
      st.write("abstype ");
      dat_binds(st, cfg, dec.dat_binds())?;
      if let Some(withtype) = dec.with_type() {
        st.write("\n");
        cfg.output_indent(st);
        st.write("withtype ");
        ty_binds(st, cfg, withtype.ty_binds())?;
      }
      st.write(" with ");
      get_dec(st, cfg, dec.decs())?;
      st.write(" end");
    }
    ast::DecOne::ExDec(dec) => {
      st.write("exception ");
      sep_with_lines(st, cfg, "and ", dec.ex_binds(), |st, ex_bind| {
        st.write(ex_bind.name_star_eq()?.token.text());
        match ex_bind.ex_bind_inner() {
          Some(inner) => match inner {
            ast::ExBindInner::OfTy(of_ty) => {
              st.write(" of ");
              get_ty(st, of_ty.ty()?)
            }
            ast::ExBindInner::EqExp(eq_exp) => {
              st.write(" = ");
              get_exp(st, cfg, eq_exp.exp()?)
            }
          },
          None => Some(()),
        }
      })?;
    }
    ast::DecOne::OpenDec(dec) => {
      st.write("open ");
      sep(st, " ", dec.paths(), path)?;
    }
    ast::DecOne::InfixDec(dec) => {
      st.write("infix ");
      if let Some(int_lit) = dec.int_lit() {
        st.write(int_lit.text());
        st.write(" ");
      }
      names_space(st, dec.name_star_eqs())?;
    }
    ast::DecOne::InfixrDec(dec) => {
      st.write("infixr ");
      if let Some(int_lit) = dec.int_lit() {
        st.write(int_lit.text());
        st.write(" ");
      }
      names_space(st, dec.name_star_eqs())?;
    }
    ast::DecOne::NonfixDec(dec) => {
      st.write("nonfix ");
      names_space(st, dec.name_star_eqs())?;
    }
    ast::DecOne::DoDec(dec) => {
      st.write("do ");
      get_exp(st, cfg, dec.exp()?)?;
    }
    ast::DecOne::LocalDec(dec) => in_end(
      st,
      cfg,
      "local",
      |st, cfg| get_dec(st, cfg, dec.local_dec_hd().into_iter().flat_map(|x| x.decs())),
      |st, cfg| get_dec(st, cfg, dec.local_dec_tl().into_iter().flat_map(|x| x.decs())),
    )?,
    ast::DecOne::StructureDec(dec) => {
      st.write("structure ");
      sep_with_lines(st, cfg, "and ", dec.str_binds(), |st, str_bind| {
        st.write(str_bind.name()?.text());
        if let Some(tail) = str_bind.ascription_tail() {
          ascription_tail(st, cfg, tail)?;
        }
        if let Some(eq_str_exp) = str_bind.eq_str_exp() {
          st.write(" = ");
          get_str_exp(st, cfg, eq_str_exp.str_exp()?)?;
        }
        Some(())
      })?;
    }
    ast::DecOne::SignatureDec(dec) => {
      st.write("signature ");
      sep_with_lines(st, cfg, "and ", dec.sig_binds(), |st, sig_bind| {
        st.write(sig_bind.name()?.text());
        st.write(" = ");
        get_sig_exp(st, cfg, sig_bind.sig_exp()?)
      })?;
    }
    ast::DecOne::FunctorDec(dec) => {
      st.write("functor ");
      sep_with_lines(st, cfg, "and ", dec.functor_binds(), |st, functor_bind| {
        st.write(functor_bind.functor_name()?.text());
        st.write(" (");
        for arg in functor_bind.functor_args() {
          match arg {
            ast::FunctorArg::FunctorArgNameSigExp(arg) => {
              st.write(arg.name()?.text());
              st.write(" : ");
              get_sig_exp(st, cfg, arg.sig_exp()?)?;
            }
            ast::FunctorArg::Dec(arg) => {
              st.write("\n");
              let new_cfg = cfg.indented();
              new_cfg.output_indent(st);
              get_dec(st, new_cfg, std::iter::once(arg))?;
              st.write("\n");
            }
          }
        }
        st.write(")");
        if let Some(tail) = functor_bind.ascription_tail() {
          ascription_tail(st, cfg, tail)?;
        }
        st.write(" = ");
        get_str_exp(st, cfg, functor_bind.body()?)
      })?;
    }
    ast::DecOne::ExpDec(dec) => get_exp(st, cfg, dec.exp()?)?,
    ast::DecOne::IncludeDec(dec) => {
      st.write("include ");
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
    st.write(n.token.text());
    Some(())
  })
}

fn dat_binds<I>(st: &mut St, cfg: Cfg, iter: I) -> Res
where
  I: Iterator<Item = ast::DatBind>,
{
  sep_with_lines(st, cfg, "and ", iter, |st, dat_bind| {
    ty_var_seq(st, dat_bind.ty_var_seq())?;
    st.write(dat_bind.name()?.text());
    let eq_con_binds = dat_bind.eq_con_binds()?;
    if eq_con_binds.con_binds().count() > 1 {
      st.write(" =\n");
      cfg.indented().output_indent(st);
      sep_with_lines(st, cfg, "| ", eq_con_binds.con_binds(), con_bind)
    } else {
      st.write(" = ");
      sep(st, " | ", eq_con_binds.con_binds(), con_bind)
    }
  })
}

fn con_bind(st: &mut St, con_bind: ast::ConBind) -> Res {
  st.write(con_bind.name_star_eq()?.token.text());
  if let Some(of_ty) = con_bind.of_ty() {
    st.write(" of ");
    get_ty(st, of_ty.ty()?)?;
  }
  Some(())
}

fn ascription_tail(st: &mut St, cfg: Cfg, tail: ast::AscriptionTail) -> Res {
  st.write(" ");
  st.write(tail.ascription()?.token.text());
  st.write(" ");
  get_sig_exp(st, cfg, tail.sig_exp()?)
}

fn get_str_exp(st: &mut St, cfg: Cfg, str_exp: ast::StrExp) -> Res {
  match str_exp {
    ast::StrExp::StructStrExp(exp) => {
      st.write("struct\n");
      let new_cfg = cfg.indented().extra_blank(true);
      new_cfg.output_indent(st);
      get_dec(st, new_cfg, exp.decs())?;
      st.write("\n");
      cfg.output_indent(st);
      st.write("end");
    }
    ast::StrExp::PathStrExp(exp) => path(st, exp.path()?)?,
    ast::StrExp::AscriptionStrExp(exp) => {
      get_str_exp(st, cfg, exp.str_exp()?)?;
      ascription_tail(st, cfg, exp.ascription_tail()?)?;
    }
    ast::StrExp::AppStrExp(exp) => {
      st.write(exp.name()?.text());
      st.write(" (");
      let mut needs_end_indent = false;
      for arg in exp.app_str_exp_args() {
        match arg {
          ast::AppStrExpArg::AppStrExpArgStrExp(arg) => get_str_exp(st, cfg, arg.str_exp()?)?,
          ast::AppStrExpArg::Dec(arg) => {
            needs_end_indent = true;
            st.write("\n");
            let new_cfg = cfg.indented();
            new_cfg.output_indent(st);
            get_dec(st, new_cfg, std::iter::once(arg))?;
            st.write("\n");
          }
        }
      }
      if needs_end_indent {
        cfg.output_indent(st);
      }
      st.write(")");
    }
    ast::StrExp::LetStrExp(exp) => in_end(
      st,
      cfg,
      "let",
      |st, cfg| get_dec(st, cfg, exp.decs()),
      |st, cfg| get_str_exp(st, cfg, exp.str_exp()?),
    )?,
  }
  Some(())
}

fn get_sig_exp(st: &mut St, cfg: Cfg, sig_exp: ast::SigExp) -> Res {
  match sig_exp {
    ast::SigExp::SigSigExp(exp) => {
      st.write("sig\n");
      let new_cfg = cfg.indented();
      new_cfg.output_indent(st);
      get_dec(st, new_cfg, exp.decs())?;
      st.write("\n");
      cfg.output_indent(st);
      st.write("end");
    }
    ast::SigExp::NameSigExp(exp) => st.write(exp.name()?.text()),
    ast::SigExp::WhereTypeSigExp(exp) => {
      get_sig_exp(st, cfg, exp.sig_exp()?)?;
      st.write(" where type ");
      ty_var_seq(st, exp.ty_var_seq())?;
      path(st, exp.path()?)?;
      st.write(" = ");
      get_ty(st, exp.ty()?)?;
    }
    ast::SigExp::WhereSigExp(exp) => {
      get_sig_exp(st, cfg, exp.sig_exp()?)?;
      st.write(" where ");
      path(st, exp.lhs()?)?;
      st.write(" = ");
      path(st, exp.rhs()?)?;
    }
  }
  Some(())
}

fn ty_annotation(st: &mut St, ty_ann: Option<ast::TyAnnotation>) -> Res {
  match ty_ann {
    Some(ty_ann) => {
      st.write(" : ");
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
    st.write(ty_bind.name()?.text());
    if let Some(eq_ty) = ty_bind.eq_ty() {
      st.write(" = ");
      get_ty(st, eq_ty.ty()?)?;
    }
    Some(())
  })
}

/// if there was a ty var seq, this'll also add a space after it.
fn ty_var_seq(st: &mut St, tvs: Option<ast::TyVarSeq>) -> Res {
  let Some(tvs) = tvs else { return Some(()) };
  let parens = tvs.l_round().is_some();
  if parens {
    st.write("(");
  }
  sep(st, ", ", tvs.ty_var_args(), |st, tv| {
    st.write(tv.ty_var()?.text());
    Some(())
  })?;
  if parens {
    st.write(")");
  }
  st.write(" ");
  Some(())
}

fn body_exp_needs_newline(exp: &ast::Exp) -> bool {
  match exp {
    ast::Exp::SeqExp(_)
    | ast::Exp::LetExp(_)
    | ast::Exp::HandleExp(_)
    | ast::Exp::IfExp(_)
    | ast::Exp::WhileExp(_)
    | ast::Exp::CaseExp(_) => true,
    ast::Exp::ParenExp(exp) => exp.exp().as_ref().is_some_and(body_exp_needs_newline),
    _ => false,
  }
}

fn get_body_exp(st: &mut St, cfg: Cfg, exp: ast::Exp) -> Res {
  get_body_exp_level(st, cfg, 1, exp)
}

fn get_body_exp_level(st: &mut St, mut cfg: Cfg, level: usize, exp: ast::Exp) -> Res {
  if body_exp_needs_newline(&exp) {
    st.write("\n");
    for _ in 0..level {
      cfg = cfg.indented();
    }
    cfg.output_indent(st);
    get_exp(st, cfg, exp)
  } else {
    st.write(" ");
    get_exp(st, cfg, exp)
  }
}

fn get_exp(st: &mut St, cfg: Cfg, exp: ast::Exp) -> Res {
  match exp {
    ast::Exp::HoleExp(_) => st.write("..."),
    ast::Exp::WildcardExp(_) => st.write("_"),
    ast::Exp::OpAndalsoExp(_) => st.write("op andalso"),
    ast::Exp::OpOrelseExp(_) => st.write("op orelse"),
    ast::Exp::SConExp(exp) => st.write(exp.s_con()?.token.text()),
    ast::Exp::PathExp(exp) => op_path(st, exp.op_kw(), exp.path()?)?,
    ast::Exp::RecordExp(exp) => {
      st.write("{ ");
      sep(st, ", ", exp.exp_rows(), |st, row| {
        st.write(row.lab()?.token.text());
        if let Some(eq_exp) = row.eq_exp() {
          st.write(" = ");
          get_exp(st, cfg, eq_exp.exp()?)?;
        }
        Some(())
      })?;
      st.write(" }");
    }
    ast::Exp::SelectorExp(exp) => {
      st.write("#");
      st.write(exp.lab()?.token.text());
    }
    ast::Exp::ParenExp(exp) => {
      st.write("(");
      get_exp(st, cfg, exp.exp()?)?;
      st.write(")");
    }
    ast::Exp::TupleExp(exp) => {
      st.write("(");
      exp_args(st, cfg, exp.exp_args())?;
      st.write(")");
    }
    ast::Exp::ListExp(exp) => {
      st.write("[");
      exp_args(st, cfg, exp.exp_args())?;
      st.write("]");
    }
    ast::Exp::VectorExp(exp) => {
      st.write("#[");
      exp_args(st, cfg, exp.list_exp()?.exp_args())?;
      st.write("]");
    }
    ast::Exp::SeqExp(exp) => {
      st.write("(\n");
      let new_cfg = cfg.indented();
      new_cfg.output_indent(st);
      exp_semi_seq(st, new_cfg, false, exp.exps_in_seq())?;
      st.write("\n");
      cfg.output_indent(st);
      st.write(")");
    }
    ast::Exp::LetExp(exp) => in_end(
      st,
      cfg,
      "let",
      |st, cfg| get_dec(st, cfg, exp.decs()),
      |st, cfg| exp_semi_seq(st, cfg, true, exp.exps_in_seq()),
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
        st.write(" ");
      }
      get_exp(st, cfg, arg)?;
    }
    ast::Exp::InfixExp(exp) => {
      let operator = exp.name_star_eq()?;
      let operator = operator.token.text();
      get_exp(st, cfg, exp.lhs()?)?;
      if operator == "|>" {
        st.write("\n");
        cfg.indented().output_indent(st);
        st.write("|> ");
      } else {
        st.write(" ");
        st.write(operator);
        st.write(" ");
      }
      get_exp(st, cfg, exp.rhs()?)?;
    }
    ast::Exp::TypedExp(exp) => {
      get_exp(st, cfg, exp.exp()?)?;
      st.write(" : ");
      get_ty(st, exp.ty()?)?;
    }
    ast::Exp::AndalsoExp(exp) => {
      get_exp(st, cfg, exp.lhs()?)?;
      st.write(" andalso ");
      get_exp(st, cfg, exp.rhs()?)?;
    }
    ast::Exp::OrelseExp(exp) => {
      get_exp(st, cfg, exp.lhs()?)?;
      st.write(" orelse ");
      get_exp(st, cfg, exp.rhs()?)?;
    }
    ast::Exp::HandleExp(exp) => {
      get_exp(st, cfg, exp.exp()?)?;
      st.write(" handle\n");
      get_matcher_across_lines(st, cfg, exp.matcher())?;
    }
    ast::Exp::RaiseExp(exp) => {
      st.write("raise ");
      get_exp(st, cfg, exp.exp()?)?;
    }
    ast::Exp::IfExp(exp) => {
      st.write("if ");
      get_exp(st, cfg, exp.cond()?)?;
      st.write(" then\n");
      let new_cfg = cfg.indented();
      new_cfg.output_indent(st);
      get_exp(st, new_cfg, exp.yes()?)?;
      st.write("\n");
      cfg.output_indent(st);
      st.write("else");
      let no = exp.no()?;
      if matches!(no, ast::Exp::IfExp(_)) {
        st.write(" ");
        get_exp(st, cfg, no)?;
      } else {
        st.write("\n");
        new_cfg.output_indent(st);
        get_exp(st, new_cfg, no)?;
      }
    }
    ast::Exp::WhileExp(exp) => {
      st.write("while ");
      get_exp(st, cfg, exp.cond()?)?;
      st.write(" do ");
      get_exp(st, cfg, exp.body()?)?;
    }
    ast::Exp::CaseExp(exp) => {
      st.write("case ");
      get_exp(st, cfg, exp.exp()?)?;
      st.write(" of\n");
      get_matcher_across_lines(st, cfg, exp.matcher())?;
    }
    ast::Exp::FnExp(exp) => {
      st.write("fn ");
      sep(st, " | ", exp.matcher()?.arms(), |st, arm| get_matcher_arm(st, cfg, arm))?;
    }
  }
  Some(())
}

fn get_matcher_across_lines(st: &mut St, cfg: Cfg, matcher: Option<ast::Matcher>) -> Res {
  let Some(matcher) = matcher else {
    // we allow formatting no matcher, even though it's a parse error.
    return Some(());
  };
  cfg.indented().output_indent(st);
  sep_with_lines(st, cfg, "| ", matcher.arms(), |st, arm| get_matcher_arm(st, cfg, arm))
}

fn get_matcher_arm(st: &mut St, cfg: Cfg, arm: ast::Arm) -> Res {
  get_pat(st, arm.pat()?)?;
  st.write(" =>");
  get_body_exp_level(st, cfg, 2, arm.exp()?)
}

fn get_pat(st: &mut St, pat: ast::Pat) -> Res {
  match pat {
    ast::Pat::WildcardPat(_) => st.write("_"),
    ast::Pat::SConPat(pat) => st.write(pat.s_con()?.token.text()),
    ast::Pat::ConPat(pat) => {
      op_path(st, pat.op_kw(), pat.path()?)?;
      if let Some(pat) = pat.pat() {
        st.write(" ");
        get_pat(st, pat)?;
      }
    }
    ast::Pat::RecordPat(pat) => {
      st.write("{ ");
      sep(st, ", ", pat.pat_rows(), |st, row| {
        match row.pat_row_inner()? {
          ast::PatRowInner::RestPatRow(_) => st.write("..."),
          ast::PatRowInner::LabAndPatPatRow(row) => {
            st.write(row.lab()?.token.text());
            st.write(" = ");
            get_pat(st, row.pat()?)?;
          }
          ast::PatRowInner::LabPatRow(row) => {
            st.write(row.name_star_eq()?.token.text());
            ty_annotation(st, row.ty_annotation())?;
            if let Some(tail) = row.as_pat_tail() {
              st.write(" as ");
              get_pat(st, tail.pat()?)?;
            }
          }
        }
        Some(())
      })?;
      st.write(" }");
    }
    ast::Pat::ParenPat(pat) => {
      st.write("(");
      get_pat(st, pat.pat()?)?;
      st.write(")");
    }
    ast::Pat::TuplePat(pat) => {
      st.write("(");
      pat_args(st, pat.pat_args())?;
      st.write(")");
    }
    ast::Pat::ListPat(pat) => {
      st.write("[");
      pat_args(st, pat.pat_args())?;
      st.write("]");
    }
    ast::Pat::VectorPat(pat) => {
      st.write("#[");
      pat_args(st, pat.list_pat()?.pat_args())?;
      st.write("]");
    }
    ast::Pat::InfixPat(pat) => {
      get_pat(st, pat.lhs()?)?;
      st.write(" ");
      st.write(pat.name_star_eq()?.token.text());
      st.write(" ");
      get_pat(st, pat.rhs()?)?;
    }
    ast::Pat::TypedPat(pat) => {
      get_pat(st, pat.pat()?)?;
      st.write(" : ");
      get_ty(st, pat.ty()?)?;
    }
    ast::Pat::AsPat(pat) => {
      get_pat(st, pat.pat()?)?;
      st.write(" as ");
      get_pat(st, pat.as_pat_tail()?.pat()?)?;
    }
    ast::Pat::OrPat(pat) => {
      get_pat(st, pat.lhs()?)?;
      st.write(" | ");
      get_pat(st, pat.rhs()?)?;
    }
  }
  Some(())
}

fn get_ty(st: &mut St, ty: ast::Ty) -> Res {
  match ty {
    ast::Ty::HoleTy(_) => st.write("..."),
    ast::Ty::WildcardTy(_) => st.write("_"),
    ast::Ty::TyVarTy(ty) => st.write(ty.ty_var()?.text()),
    ast::Ty::RecordTy(ty) => {
      st.write("{ ");
      sep(st, ", ", ty.ty_rows(), |st, row| {
        st.write(row.lab()?.token.text());
        st.write(" : ");
        get_ty(st, row.ty()?)
      })?;
      st.write(" }");
    }
    ast::Ty::ConTy(ty) => {
      if let Some(ty_seq) = ty.ty_seq() {
        st.write("(");
        sep(st, ", ", ty_seq.ty_args(), |st, t| get_ty(st, t.ty()?))?;
        st.write(") ");
      }
      path(st, ty.path()?)?;
    }
    ast::Ty::OneArgConTy(ty) => {
      get_ty(st, ty.ty()?)?;
      st.write(" ");
      path(st, ty.path()?)?;
    }
    ast::Ty::TupleTy(ty) => {
      get_ty(st, ty.ty()?);
      for ty in ty.star_tys() {
        st.write(" * ");
        get_ty(st, ty.ty()?)?;
      }
    }
    ast::Ty::FnTy(ty) => {
      get_ty(st, ty.param()?)?;
      st.write(" -> ");
      get_ty(st, ty.res()?)?;
    }
    ast::Ty::ParenTy(ty) => {
      st.write("(");
      get_ty(st, ty.ty()?)?;
      st.write(")");
    }
  }
  Some(())
}

fn in_end<F1, F2>(st: &mut St, cfg: Cfg, kw: &str, f1: F1, f2: F2) -> Res
where
  F1: FnOnce(&mut St, Cfg) -> Res,
  F2: FnOnce(&mut St, Cfg) -> Res,
{
  st.write(kw);
  st.write("\n");
  let new_cfg = cfg.indented();
  new_cfg.output_indent(st);
  f1(st, new_cfg)?;
  st.write("\n");
  cfg.output_indent(st);
  st.write("in\n");
  new_cfg.output_indent(st);
  f2(st, new_cfg)?;
  st.write("\n");
  cfg.output_indent(st);
  st.write("end");
  Some(())
}

fn path(st: &mut St, p: ast::Path) -> Res {
  sep(st, ".", p.name_star_eq_dots(), |st, x| {
    st.write(x.name_star_eq()?.token.text());
    Some(())
  })
}

fn op_path(
  st: &mut St,
  op: Option<sml_syntax::kind::SyntaxToken>,
  p: sml_syntax::ast::Path,
) -> Res {
  if op.is_some() {
    st.write("op");
    let needs_space = p
      .name_star_eq_dots()
      .next()
      .and_then(|x| x.name_star_eq()?.token.text().as_bytes().first().copied())
      .is_some_and(|x| x.is_ascii_alphanumeric());
    if needs_space {
      st.write(" ");
    }
  }
  path(st, p)
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
    st.write(s);
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
    st.write("\n");
    if cfg.extra_blank {
      st.write("\n");
    }
    cfg.output_indent(st);
    st.write(s);
    get_t(st, arg)?;
  }
  Some(())
}

fn exp_semi_seq<I>(st: &mut St, cfg: Cfg, allow_last_semi: bool, mut iter: I) -> Res
where
  I: Iterator<Item = ast::ExpInSeq>,
{
  if let Some(e) = iter.next() {
    get_exp(st, cfg, e.exp()?)?;
  }
  let mut last = false;
  for e in iter {
    st.write(";\n");
    cfg.output_indent(st);
    get_exp(st, cfg, e.exp()?)?;
    last = e.semicolon().is_some();
  }
  if last && allow_last_semi {
    st.write(";");
  }
  Some(())
}
