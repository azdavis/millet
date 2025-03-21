//! Lowering expressions.

use crate::common::{forbid_opaque_asc, get_lab, get_path, get_scon};
use crate::util::{Disallowed, ErrorKind, MatcherFlavor, MissingRhs, Sep, St};
use crate::{dec, pat, ty};
use sml_syntax::ast::{self, AstNode as _, SyntaxNodePtr};

pub(crate) fn get(st: &mut St<'_>, exp: Option<ast::Exp>) -> sml_hir::ExpIdx {
  let exp = exp?;
  let ptr = SyntaxNodePtr::new(exp.syntax());
  let ret = match exp {
    ast::Exp::HoleExp(_) | ast::Exp::WildcardExp(_) => sml_hir::Exp::Hole,
    ast::Exp::OpAndalsoExp(_) | ast::Exp::OpOrelseExp(_) => {
      st.err(exp.syntax(), ErrorKind::OpBoolBinOp);
      return None;
    }
    ast::Exp::SConExp(exp) => sml_hir::Exp::SCon(get_scon(st, exp.s_con()?)?),
    ast::Exp::PathExp(exp) => {
      if !st.lang().exp.path {
        st.err(exp.syntax(), ErrorKind::Disallowed(Disallowed::Exp("path")));
      }
      sml_hir::Exp::Path(get_path(exp.path()?)?)
    }
    ast::Exp::RecordExp(exp) => {
      if !st.lang().exp.record {
        st.err(exp.syntax(), ErrorKind::Disallowed(Disallowed::Exp("record")));
      }
      if let Some(s) = exp.exp_rows().last().and_then(|x| x.comma()) {
        st.err_tok(&s, ErrorKind::Trailing(Sep::Comma));
      }
      let rows = exp.exp_rows().filter_map(|row| {
        let lab_ast = row.lab()?;
        let tok = row.lab()?.token;
        let lab = get_lab(st, lab_ast);
        let exp = match row.eq_exp() {
          Some(eq_exp) => get(st, eq_exp.exp()),
          None => match &lab {
            sml_hir::Lab::Name(name) => {
              if !st.lang().successor_ml.exp_row_pun {
                let e = ErrorKind::Disallowed(Disallowed::SuccMl("expression row punning"));
                st.err_tok(&tok, e);
              }
              st.exp(sml_hir::Exp::Path(sml_path::Path::one(name.clone())), ptr)
            }
            sml_hir::Lab::Num(_) => {
              st.err_tok(&tok, ErrorKind::MissingRhs(MissingRhs::ExpRow));
              None
            }
          },
        };
        Some((lab, exp))
      });
      let rows: Vec<_> = rows.collect();
      if rows.is_empty() {
        st.err(exp.syntax(), ErrorKind::EmptyRecordPatOrExp);
      }
      sml_hir::Exp::Record(rows)
    }
    ast::Exp::SelectorExp(exp) => {
      if !st.lang().exp.selector {
        st.err(exp.syntax(), ErrorKind::Disallowed(Disallowed::Exp("`#` selector")));
      }
      let lab = get_lab(st, exp.lab()?);
      let fresh = st.fresh();
      let pat = st.pat(pat::name(fresh.as_str()), ptr);
      let param = st.pat(sml_hir::Pat::Record { rows: vec![(lab, pat)], allows_other: true }, ptr);
      let body = st.exp(name(fresh.as_str()), ptr);
      let arm = sml_hir::Arm { pat: param, exp: body };
      sml_hir::Exp::Fn(vec![arm], sml_hir::FnFlavor::Selector)
    }
    // @def(5)
    ast::Exp::ParenExp(exp) => {
      if !st.lang().exp.paren {
        st.err(exp.syntax(), ErrorKind::Disallowed(Disallowed::Exp("parentheses")));
      }
      let inner = exp.exp();
      if inner.as_ref().is_some_and(warn_unnecessary_parens) {
        st.err(exp.syntax(), ErrorKind::UnnecessaryParens);
      }
      return get(st, inner);
    }
    ast::Exp::TupleExp(exp) => {
      if !st.lang().exp.tuple {
        st.err(exp.syntax(), ErrorKind::Disallowed(Disallowed::Exp("tuple")));
      }
      if let Some(s) = exp.exp_args().last().and_then(|x| x.comma()) {
        st.err_tok(&s, ErrorKind::Trailing(Sep::Comma));
      }
      tuple(exp.exp_args().map(|e| get(st, e.exp())))
    }
    ast::Exp::ListExp(exp) => {
      if !st.lang().exp.list {
        st.err(exp.syntax(), ErrorKind::Disallowed(Disallowed::Exp("list")));
      }
      if let Some(s) = exp.exp_args().last().and_then(|x| x.comma()) {
        st.err_tok(&s, ErrorKind::Trailing(Sep::Comma));
      }
      // need to rev()
      #[allow(clippy::needless_collect)]
      let exps: Vec<_> = exp.exp_args().map(|x| get(st, x.exp())).collect();
      exps.into_iter().rev().fold(name("nil"), |ac, x| {
        let cons = st.exp(name("::"), ptr);
        let ac = st.exp(ac, ptr);
        sml_hir::Exp::App(cons, st.exp(tuple([x, ac]), ptr))
      })
    }
    ast::Exp::VectorExp(exp) => {
      if !st.lang().successor_ml.vector {
        st.err(exp.syntax(), ErrorKind::Disallowed(Disallowed::SuccMl("vector expressions")));
      }
      let last_comma =
        exp.list_exp().into_iter().flat_map(|x| x.exp_args()).last().and_then(|x| x.comma());
      if let Some(s) = last_comma {
        st.err_tok(&s, ErrorKind::Trailing(Sep::Comma));
      }
      let exps: Vec<_> =
        exp.list_exp().into_iter().flat_map(|x| x.exp_args()).map(|x| get(st, x.exp())).collect();
      sml_hir::Exp::Vector(exps)
    }
    ast::Exp::SeqExp(exp) => {
      if !st.lang().exp.seq {
        st.err(exp.syntax(), ErrorKind::Disallowed(Disallowed::Exp("sequence")));
      }
      if let Some(s) = exp.exps_in_seq().last().and_then(|x| x.semicolon()) {
        st.err_tok(&s, ErrorKind::Trailing(Sep::Semi));
      }
      let exps: Vec<_> = exp.exps_in_seq().map(|x| get(st, x.exp())).collect();
      return exp_idx_in_seq(st, exps, exp.syntax());
    }
    ast::Exp::LetExp(exp) => {
      if !st.lang().exp.let_ {
        st.err(exp.syntax(), ErrorKind::Disallowed(Disallowed::Exp("`let`")));
      }
      st.inc_level();
      let dec = dec::get(st, exp.decs());
      st.dec_level();
      if dec.is_empty() {
        st.err(exp.syntax(), ErrorKind::EmptyLet);
      }
      let mut exps: Vec<_> = exp.exps_in_seq().map(|x| get(st, x.exp())).collect();
      if let Some(s) = exp.exps_in_seq().last().and_then(|x| x.semicolon()) {
        if !st.lang().successor_ml.opt_semi {
          let e = ErrorKind::Disallowed(Disallowed::SuccMl("trailing `;` in `let` expressions"));
          st.err_tok(&s, e);
        }
        exps.push(st.exp(tuple([]), ptr));
      }
      let exp = exp_idx_in_seq(st, exps, exp.syntax());
      sml_hir::Exp::Let(dec, exp)
    }
    ast::Exp::AppExp(exp) => {
      if !st.lang().exp.app {
        st.err(exp.syntax(), ErrorKind::Disallowed(Disallowed::Exp("function application")));
      }
      sml_hir::Exp::App(get(st, exp.func()), get(st, exp.arg()))
    }
    ast::Exp::InfixExp(exp) => {
      if !st.lang().exp.infix {
        st.err(exp.syntax(), ErrorKind::Disallowed(Disallowed::Exp("infix application")));
      }
      let func = exp.name_star_eq().and_then(|x| st.exp(name(x.token.text()), ptr));
      let lhs = get(st, exp.lhs());
      let rhs = get(st, exp.rhs());
      let arg = st.exp(tuple([lhs, rhs]), ptr);
      sml_hir::Exp::App(func, arg)
    }
    ast::Exp::TypedExp(exp) => {
      if !st.lang().exp.typed {
        st.err(exp.syntax(), ErrorKind::Disallowed(Disallowed::Exp("typed")));
      }
      forbid_opaque_asc(st, exp.ascription());
      sml_hir::Exp::Typed(get(st, exp.exp()), ty::get(st, exp.ty()), sml_hir::TypedFlavor::Regular)
    }
    ast::Exp::AndalsoExp(exp) => {
      if !st.lang().exp.andalso {
        st.err(exp.syntax(), ErrorKind::Disallowed(Disallowed::Exp("`andalso`")));
      }
      let lhs = exp.lhs();
      let rhs = exp.rhs();
      if is_bool_lit(lhs.as_ref()) || is_bool_lit(rhs.as_ref()) {
        st.err(exp.syntax(), ErrorKind::ComplexBoolExp);
      }
      let cond = get(st, lhs);
      let yes = get(st, rhs);
      let no = st.exp(name("false"), ptr);
      if_(st, cond, yes, no, ptr, sml_hir::FnFlavor::BoolBinOp)
    }
    ast::Exp::OrelseExp(exp) => {
      if !st.lang().exp.orelse {
        st.err(exp.syntax(), ErrorKind::Disallowed(Disallowed::Exp("`orelse`")));
      }
      let lhs = exp.lhs();
      let rhs = exp.rhs();
      if is_bool_lit(lhs.as_ref()) || is_bool_lit(rhs.as_ref()) {
        st.err(exp.syntax(), ErrorKind::ComplexBoolExp);
      }
      let cond = get(st, lhs);
      let yes = st.exp(name("true"), ptr);
      let no = get(st, rhs);
      if_(st, cond, yes, no, ptr, sml_hir::FnFlavor::BoolBinOp)
    }
    ast::Exp::HandleExp(exp) => {
      if !st.lang().exp.handle {
        st.err(exp.syntax(), ErrorKind::Disallowed(Disallowed::Exp("`handle`")));
      }
      let head = get(st, exp.exp());
      let m = matcher(st, Some(MatcherFlavor::Handle), exp.matcher());
      sml_hir::Exp::Handle(head, m)
    }
    ast::Exp::RaiseExp(exp) => {
      if !st.lang().exp.raise {
        st.err(exp.syntax(), ErrorKind::Disallowed(Disallowed::Exp("`raise`")));
      }
      sml_hir::Exp::Raise(get(st, exp.exp()))
    }
    ast::Exp::IfExp(exp) => {
      if !st.lang().exp.if_ {
        st.err(exp.syntax(), ErrorKind::Disallowed(Disallowed::Exp("`if`")));
      }
      let cond = exp.cond();
      let yes = exp.yes();
      let no = exp.no();
      if is_bool_lit(cond.as_ref()) || is_bool_lit(yes.as_ref()) || is_bool_lit(no.as_ref()) {
        st.err(exp.syntax(), ErrorKind::ComplexBoolExp);
      }
      let cond = get(st, cond);
      let yes = get(st, yes);
      let no = get(st, no);
      if_(st, cond, yes, no, ptr, sml_hir::FnFlavor::If)
    }
    ast::Exp::WhileExp(exp) => {
      if !st.lang().exp.while_ {
        st.err(exp.syntax(), ErrorKind::Disallowed(Disallowed::Exp("`while`")));
      }
      let vid = st.fresh();
      let fn_body = {
        let cond = get(st, exp.cond());
        let body = get(st, exp.body());
        let call = call_unit_fn(st, &vid, ptr);
        let yes = exp_idx_in_seq(st, [body, call], exp.syntax());
        let no = st.exp(tuple([]), ptr);
        let fn_body = if_(st, cond, yes, no, ptr, sml_hir::FnFlavor::While);
        st.exp(fn_body, ptr)
      };
      let arg_pat = st.pat(pat::tuple([]), ptr);
      let arm = sml_hir::Arm { pat: arg_pat, exp: fn_body };
      let fn_exp = st.exp(sml_hir::Exp::Fn(vec![arm], sml_hir::FnFlavor::While), ptr);
      let vid_pat = st.pat(pat::name(vid.as_str()), ptr);
      let bind = sml_hir::ValBind { rec: true, pat: vid_pat, exp: fn_exp };
      let val = st.dec(sml_hir::Dec::Val(vec![], vec![bind], sml_hir::ValFlavor::While), ptr);
      sml_hir::Exp::Let(vec![val], call_unit_fn(st, &vid, ptr))
    }
    ast::Exp::CaseExp(exp) => {
      if !st.lang().exp.case {
        st.err(exp.syntax(), ErrorKind::Disallowed(Disallowed::Exp("`case`")));
      }
      let head = get(st, exp.exp());
      let arms = matcher(st, Some(MatcherFlavor::Case), exp.matcher());
      if arms.len() == 1 {
        st.err(exp.syntax(), ErrorKind::OneArmedCase);
      }
      case(st, head, arms, ptr, sml_hir::FnFlavor::Case)
    }
    ast::Exp::FnExp(exp) => {
      if !st.lang().exp.fn_ {
        st.err(exp.syntax(), ErrorKind::Disallowed(Disallowed::Exp("`fn`")));
      }
      sml_hir::Exp::Fn(matcher(st, Some(MatcherFlavor::Fn), exp.matcher()), sml_hir::FnFlavor::Fn)
    }
  };
  st.exp(ret, ptr)
}

fn is_bool_lit(exp: Option<&ast::Exp>) -> bool {
  let Some(exp) = exp else { return false };
  match exp {
    ast::Exp::PathExp(exp) => exp.path().is_some_and(|p| {
      let mut iter = p.name_star_eq_dots();
      let is_true_or_false = iter
        .next()
        .and_then(|x| x.name_star_eq())
        .is_some_and(|x| matches!(x.token.text(), "true" | "false"));
      is_true_or_false && iter.next().is_none()
    }),
    ast::Exp::ParenExp(exp) => is_bool_lit(exp.exp().as_ref()),
    ast::Exp::TypedExp(exp) => is_bool_lit(exp.exp().as_ref()),
    ast::Exp::HandleExp(exp) => is_bool_lit(exp.exp().as_ref()),
    _ => false,
  }
}

/// not strictly "is atomic".
fn warn_unnecessary_parens(exp: &ast::Exp) -> bool {
  match exp {
    ast::Exp::SConExp(_)
    | ast::Exp::RecordExp(_)
    | ast::Exp::SelectorExp(_)
    | ast::Exp::ParenExp(_)
    | ast::Exp::TupleExp(_)
    | ast::Exp::ListExp(_)
    | ast::Exp::VectorExp(_)
    | ast::Exp::SeqExp(_)
    | ast::Exp::LetExp(_) => true,
    ast::Exp::PathExp(exp) => exp.op_kw().is_none(),
    ast::Exp::HoleExp(_)
    | ast::Exp::WildcardExp(_)
    | ast::Exp::OpAndalsoExp(_)
    | ast::Exp::OpOrelseExp(_)
    | ast::Exp::AppExp(_)
    | ast::Exp::InfixExp(_)
    | ast::Exp::TypedExp(_)
    | ast::Exp::AndalsoExp(_)
    | ast::Exp::OrelseExp(_)
    | ast::Exp::HandleExp(_)
    | ast::Exp::RaiseExp(_)
    | ast::Exp::IfExp(_)
    | ast::Exp::WhileExp(_)
    | ast::Exp::CaseExp(_)
    | ast::Exp::FnExp(_) => false,
  }
}

pub(crate) fn name(s: &str) -> sml_hir::Exp {
  sml_hir::Exp::Path(sml_path::Path::one(str_util::Name::new(s)))
}

pub(crate) fn tuple<I>(es: I) -> sml_hir::Exp
where
  I: IntoIterator<Item = sml_hir::ExpIdx>,
{
  let rows: Vec<_> =
    es.into_iter().enumerate().map(|(idx, e)| (sml_hir::Lab::tuple(idx), e)).collect();
  // can't assert rows.len() != 1 because of this
  cov_mark::hit("trailing_exp_arg");
  sml_hir::Exp::Record(rows)
}

fn call_unit_fn(st: &mut St<'_>, vid: &str_util::Name, ptr: SyntaxNodePtr) -> sml_hir::ExpIdx {
  let vid_exp = st.exp(name(vid.as_str()), ptr);
  let arg_exp = st.exp(sml_hir::Exp::Record(vec![]), ptr);
  st.exp(sml_hir::Exp::App(vid_exp, arg_exp), ptr)
}

/// lowers 1 into 2. (which is then lowered into 3.)
///
/// 1. `(e1; ...; en; e)`
/// 2. `(case e1 of _ => ... => case en of _ => e)`
/// 3. `((fn _ => ... (fn _ => (fn _ => e) en) ...) e1)`
///
/// the vec must not be empty, since we need a last expression `e`.
fn exp_idx_in_seq<A, B>(
  st: &mut St<'_>,
  exps: A,
  exp: &sml_syntax::kind::SyntaxNode,
) -> sml_hir::ExpIdx
where
  A: IntoIterator<IntoIter = B>,
  B: DoubleEndedIterator<Item = sml_hir::ExpIdx>,
{
  let ptr = SyntaxNodePtr::new(exp);
  let ret = exps.into_iter().rev().reduce(|ac, x| {
    let wild = st.pat(sml_hir::Pat::Wild, ptr);
    let arm = sml_hir::Arm { pat: wild, exp: ac };
    let c = case(st, x, vec![arm], ptr, sml_hir::FnFlavor::Seq);
    st.exp(c, ptr)
  });
  if ret.is_none() {
    st.err(exp, ErrorKind::EmptyExpSemiSeq);
  }
  ret.flatten()
}

fn if_(
  st: &mut St<'_>,
  cond: sml_hir::ExpIdx,
  yes: sml_hir::ExpIdx,
  no: sml_hir::ExpIdx,
  ptr: SyntaxNodePtr,
  flavor: sml_hir::FnFlavor,
) -> sml_hir::Exp {
  let yes_pat = st.pat(pat::name("true"), ptr);
  let no_pat = st.pat(pat::name("false"), ptr);
  let yes_arm = sml_hir::Arm { pat: yes_pat, exp: yes };
  let no_arm = sml_hir::Arm { pat: no_pat, exp: no };
  case(st, cond, vec![yes_arm, no_arm], ptr, flavor)
}

pub(crate) fn case(
  st: &mut St<'_>,
  head: sml_hir::ExpIdx,
  arms: Vec<sml_hir::Arm>,
  ptr: SyntaxNodePtr,
  flavor: sml_hir::FnFlavor,
) -> sml_hir::Exp {
  sml_hir::Exp::App(st.exp(sml_hir::Exp::Fn(arms, flavor), ptr), head)
}

fn matcher(
  st: &mut St<'_>,
  flavor: Option<MatcherFlavor>,
  matcher: Option<ast::Matcher>,
) -> Vec<sml_hir::Arm> {
  let bar = matcher.as_ref().and_then(sml_syntax::ast::Matcher::bar);
  if let (false, Some(bar)) = (st.lang().successor_ml.opt_bar, bar) {
    let e = ErrorKind::Disallowed(Disallowed::SuccMl(
      "preceding `|` before first arm in a `case`, `fn`, or `handle`",
    ));
    st.err_tok(&bar, e);
  }
  matcher
    .into_iter()
    .flat_map(|x| x.arms())
    .map(|arm| {
      let pat = pat::get(st, flavor, arm.pat());
      let exp = get(st, arm.exp());
      sml_hir::Arm { pat, exp }
    })
    .collect()
}
