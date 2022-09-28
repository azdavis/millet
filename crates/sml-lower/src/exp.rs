use crate::common::{get_lab, get_path, get_scon};
use crate::util::{Cx, ErrorKind};
use crate::{dec, pat, ty};
use sml_syntax::ast::{self, AstNode as _, SyntaxNodePtr};

pub(crate) fn get(cx: &mut Cx, exp: Option<ast::Exp>) -> sml_hir::ExpIdx {
  let exp = exp?;
  let ptr = SyntaxNodePtr::new(exp.syntax());
  let ret = match exp {
    ast::Exp::HoleExp(_) | ast::Exp::WildcardExp(_) => sml_hir::Exp::Hole,
    ast::Exp::OpAndalsoExp(_) | ast::Exp::OpOrelseExp(_) => {
      cx.err(exp.syntax().text_range(), ErrorKind::OpBoolBinOp);
      return None;
    }
    ast::Exp::SConExp(exp) => sml_hir::Exp::SCon(get_scon(cx, exp.s_con()?)?),
    ast::Exp::PathExp(exp) => sml_hir::Exp::Path(get_path(exp.path()?)?),
    ast::Exp::RecordExp(exp) => sml_hir::Exp::Record(
      exp
        .exp_rows()
        .filter_map(|row| {
          let lab_ast = row.lab()?;
          let lab_tr = lab_ast.token.text_range();
          let lab = get_lab(cx, lab_ast);
          let exp = match row.eq_exp() {
            Some(eq_exp) => get(cx, eq_exp.exp()),
            None => match &lab {
              sml_hir::Lab::Name(name) => {
                cx.err(lab_tr, ErrorKind::Unsupported("expression row punning"));
                cx.exp(sml_hir::Exp::Path(sml_hir::Path::one(name.clone())), ptr.clone())
              }
              sml_hir::Lab::Num(_) => {
                // NOTE: we explicitly duplicate the `err` call in both branches, to remind us that
                // if we ever actually accepted expression row punning, we should add a separate
                // error here rejecting the attempt to pun with a int label.
                cx.err(lab_tr, ErrorKind::Unsupported("expression row punning"));
                None
              }
            },
          };
          Some((lab, exp))
        })
        .collect(),
    ),
    ast::Exp::SelectorExp(exp) => {
      let lab = get_lab(cx, exp.lab()?);
      let fresh = cx.fresh();
      let pat = cx.pat(pat::name(fresh.as_str()), ptr.clone());
      let param =
        cx.pat(sml_hir::Pat::Record { rows: vec![(lab, pat)], allows_other: true }, ptr.clone());
      let body = cx.exp(name(fresh.as_str()), ptr.clone());
      sml_hir::Exp::Fn(vec![(param, body)])
    }
    // sml_def(5)
    ast::Exp::ParenExp(exp) => return get(cx, exp.exp()),
    ast::Exp::TupleExp(exp) => tuple(exp.exp_args().map(|e| get(cx, e.exp()))),
    ast::Exp::ListExp(exp) => {
      // need to rev()
      #[allow(clippy::needless_collect)]
      let exps: Vec<_> = exp.exp_args().map(|x| get(cx, x.exp())).collect();
      exps.into_iter().rev().fold(name("nil"), |ac, x| {
        let cons = cx.exp(name("::"), ptr.clone());
        let ac = cx.exp(ac, ptr.clone());
        sml_hir::Exp::App(cons, cx.exp(tuple([x, ac]), ptr.clone()))
      })
    }
    ast::Exp::VectorExp(exp) => {
      cx.err(exp.syntax().text_range(), ErrorKind::Unsupported("vector expressions"));
      return None;
    }
    ast::Exp::SeqExp(exp) => return exps_in_seq(cx, exp.exps_in_seq(), ptr),
    ast::Exp::LetExp(exp) => {
      let dec = dec::get(cx, exp.dec());
      let exp = exps_in_seq(cx, exp.exps_in_seq(), ptr.clone());
      sml_hir::Exp::Let(dec, exp)
    }
    ast::Exp::AppExp(exp) => sml_hir::Exp::App(get(cx, exp.func()), get(cx, exp.arg())),
    ast::Exp::InfixExp(exp) => {
      let func = exp.name_star_eq().and_then(|x| cx.exp(name(x.token.text()), ptr.clone()));
      let lhs = get(cx, exp.lhs());
      let rhs = get(cx, exp.rhs());
      let arg = cx.exp(tuple([lhs, rhs]), ptr.clone());
      sml_hir::Exp::App(func, arg)
    }
    ast::Exp::TypedExp(exp) => sml_hir::Exp::Typed(get(cx, exp.exp()), ty::get(cx, exp.ty())),
    ast::Exp::AndalsoExp(exp) => {
      let cond = get(cx, exp.lhs());
      let yes = get(cx, exp.rhs());
      let no = cx.exp(name("false"), ptr.clone());
      if_(cx, cond, yes, no, ptr.clone())
    }
    ast::Exp::OrelseExp(exp) => {
      let cond = get(cx, exp.lhs());
      let yes = cx.exp(name("true"), ptr.clone());
      let no = get(cx, exp.rhs());
      if_(cx, cond, yes, no, ptr.clone())
    }
    ast::Exp::HandleExp(exp) => {
      sml_hir::Exp::Handle(get(cx, exp.exp()), matcher(cx, exp.matcher()))
    }
    ast::Exp::RaiseExp(exp) => sml_hir::Exp::Raise(get(cx, exp.exp())),
    ast::Exp::IfExp(exp) => {
      let cond = get(cx, exp.cond());
      let yes = get(cx, exp.yes());
      let no = get(cx, exp.no());
      if_(cx, cond, yes, no, ptr.clone())
    }
    ast::Exp::WhileExp(exp) => {
      let vid = cx.fresh();
      let fn_body = {
        let cond = get(cx, exp.cond());
        let body = get(cx, exp.body());
        let call = call_unit_fn(cx, &vid, ptr.clone());
        let yes = exp_idx_in_seq(cx, [body, call], ptr.clone());
        let no = cx.exp(tuple([]), ptr.clone());
        let fn_body = if_(cx, cond, yes, no, ptr.clone());
        cx.exp(fn_body, ptr.clone())
      };
      let arg_pat = cx.pat(pat::tuple([]), ptr.clone());
      let fn_exp = cx.exp(sml_hir::Exp::Fn(vec![(arg_pat, fn_body)]), ptr.clone());
      let vid_pat = cx.pat(pat::name(vid.as_str()), ptr.clone());
      let val = cx.dec(
        sml_hir::Dec::Val(vec![], vec![sml_hir::ValBind { rec: true, pat: vid_pat, exp: fn_exp }]),
        ptr.clone(),
      );
      sml_hir::Exp::Let(val, call_unit_fn(cx, &vid, ptr.clone()))
    }
    ast::Exp::CaseExp(exp) => {
      let head = get(cx, exp.exp());
      let arms = matcher(cx, exp.matcher());
      case(cx, head, arms, ptr.clone())
    }
    ast::Exp::FnExp(exp) => sml_hir::Exp::Fn(matcher(cx, exp.matcher())),
  };
  cx.exp(ret, ptr)
}

pub(crate) fn name(s: &str) -> sml_hir::Exp {
  sml_hir::Exp::Path(sml_hir::Path::one(str_util::Name::new(s)))
}

pub(crate) fn tuple<I>(es: I) -> sml_hir::Exp
where
  I: IntoIterator<Item = sml_hir::ExpIdx>,
{
  let rows: Vec<_> =
    es.into_iter().enumerate().map(|(idx, e)| (sml_hir::Lab::tuple(idx), e)).collect();
  assert_ne!(rows.len(), 1);
  sml_hir::Exp::Record(rows)
}

fn call_unit_fn(cx: &mut Cx, vid: &str_util::Name, ptr: SyntaxNodePtr) -> sml_hir::ExpIdx {
  let vid_exp = cx.exp(name(vid.as_str()), ptr.clone());
  let arg_exp = cx.exp(sml_hir::Exp::Record(vec![]), ptr.clone());
  cx.exp(sml_hir::Exp::App(vid_exp, arg_exp), ptr)
}

fn exps_in_seq<I>(cx: &mut Cx, exps: I, ptr: SyntaxNodePtr) -> sml_hir::ExpIdx
where
  I: Iterator<Item = ast::ExpInSeq>,
{
  let exps: Vec<_> = exps.into_iter().map(|e| get(cx, e.exp())).collect();
  exp_idx_in_seq(cx, exps, ptr)
}

/// lowers 1 into 2. (which is then lowered into 3.)
///
/// 1. `(e1; ...; en; e)`
/// 2. `(case e1 of _ => ... => case en of _ => e)`
/// 3. `((fn _ => ... (fn _ => (fn _ => e) en) ...) e1)`
///
/// the vec must not be empty, since we need a last expression `e`.
fn exp_idx_in_seq<A, B>(cx: &mut Cx, exps: A, ptr: SyntaxNodePtr) -> sml_hir::ExpIdx
where
  A: IntoIterator<IntoIter = B>,
  B: DoubleEndedIterator<Item = sml_hir::ExpIdx>,
{
  exps
    .into_iter()
    .rev()
    .reduce(|ac, x| {
      let wild = cx.pat(sml_hir::Pat::Wild, ptr.clone());
      let c = case(cx, x, vec![(wild, ac)], ptr.clone());
      cx.exp(c, ptr.clone())
    })
    .flatten()
}

fn if_(
  cx: &mut Cx,
  cond: sml_hir::ExpIdx,
  yes: sml_hir::ExpIdx,
  no: sml_hir::ExpIdx,
  ptr: SyntaxNodePtr,
) -> sml_hir::Exp {
  let yes_pat = cx.pat(pat::name("true"), ptr.clone());
  let no_pat = cx.pat(pat::name("false"), ptr.clone());
  case(cx, cond, vec![(yes_pat, yes), (no_pat, no)], ptr)
}

pub(crate) fn case(
  cx: &mut Cx,
  head: sml_hir::ExpIdx,
  arms: Vec<(sml_hir::PatIdx, sml_hir::ExpIdx)>,
  ptr: SyntaxNodePtr,
) -> sml_hir::Exp {
  sml_hir::Exp::App(cx.exp(sml_hir::Exp::Fn(arms), ptr), head)
}

fn matcher(cx: &mut Cx, matcher: Option<ast::Matcher>) -> Vec<(sml_hir::PatIdx, sml_hir::ExpIdx)> {
  if let Some(bar) = matcher.as_ref().and_then(|m| m.bar()) {
    cx.err(bar.text_range(), ErrorKind::PrecedingBar);
  }
  matcher
    .into_iter()
    .flat_map(|x| x.match_rules())
    .map(|arm| (pat::get(cx, arm.pat()), get(cx, arm.exp())))
    .collect()
}
