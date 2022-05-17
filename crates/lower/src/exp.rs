use crate::common::{get_lab, get_path, get_scon};
use crate::util::Cx;
use crate::{dec, pat, ty};
use syntax::ast;

pub(crate) fn get(cx: &mut Cx, exp: Option<ast::Exp>) -> hir::ExpIdx {
  let exp = exp.and_then(|e| get_(cx, e)).unwrap_or(hir::Exp::None);
  cx.arenas.exp.alloc(exp)
}

fn get_(cx: &mut Cx, exp: ast::Exp) -> Option<hir::Exp> {
  let ret = match exp {
    ast::Exp::SConExp(exp) => hir::Exp::SCon(get_scon(exp.s_con()?)?),
    ast::Exp::PathExp(exp) => hir::Exp::Path(get_path(exp.path()?)?),
    ast::Exp::RecordExp(exp) => hir::Exp::Record(
      exp
        .exp_rows()
        .filter_map(|row| Some((get_lab(row.lab()?)?, get(cx, row.exp()))))
        .collect(),
    ),
    ast::Exp::SelectorExp(exp) => {
      let lab = get_lab(exp.lab()?)?;
      let fresh = cx.fresh();
      let pat = cx.arenas.pat.alloc(pat::name(fresh.as_str()));
      let param = cx.arenas.pat.alloc(hir::Pat::Record {
        rows: vec![(lab, pat)],
        allows_other: true,
      });
      let body = cx.arenas.exp.alloc(name(fresh.as_str()));
      hir::Exp::Fn(vec![(param, body)])
    }
    ast::Exp::TupleExp(exp) => tuple(exp.exp_args().map(|e| get(cx, e.exp()))),
    ast::Exp::ListExp(exp) => {
      // need to rev()
      #[allow(clippy::needless_collect)]
      let exps: Vec<_> = exp.exp_args().map(|x| get(cx, x.exp())).collect();
      exps.into_iter().rev().fold(name("nil"), |ac, x| {
        let cons = cx.arenas.exp.alloc(name("::"));
        let ac = cx.arenas.exp.alloc(ac);
        hir::Exp::App(cons, cx.arenas.exp.alloc(tuple([x, ac])))
      })
    }
    ast::Exp::SeqExp(exp) => exps_in_seq(cx, exp.exps_in_seq()),
    ast::Exp::LetExp(exp) => {
      let dec = dec::get(cx, exp.dec());
      let exp = exps_in_seq(cx, exp.exps_in_seq());
      hir::Exp::Let(dec, cx.arenas.exp.alloc(exp))
    }
    ast::Exp::AppExp(exp) => hir::Exp::App(get(cx, exp.func()), get(cx, exp.arg())),
    ast::Exp::InfixExp(exp) => {
      let func = exp
        .name()
        .map(|tok| name(tok.text()))
        .unwrap_or(hir::Exp::None);
      let func = cx.arenas.exp.alloc(func);
      let lhs = get(cx, exp.lhs());
      let rhs = get(cx, exp.rhs());
      let arg = cx.arenas.exp.alloc(tuple([lhs, rhs]));
      hir::Exp::App(func, arg)
    }
    ast::Exp::TypedExp(exp) => hir::Exp::Typed(get(cx, exp.exp()), ty::get(cx, exp.ty())),
    ast::Exp::AndalsoExp(exp) => {
      let cond = get(cx, exp.lhs());
      let yes = get(cx, exp.rhs());
      let no = cx.arenas.exp.alloc(name("false"));
      if_(cx, cond, yes, no)
    }
    ast::Exp::OrelseExp(exp) => {
      let cond = get(cx, exp.lhs());
      let yes = cx.arenas.exp.alloc(name("true"));
      let no = get(cx, exp.rhs());
      if_(cx, cond, yes, no)
    }
    ast::Exp::HandleExp(exp) => hir::Exp::Handle(get(cx, exp.exp()), matcher(cx, exp.matcher())),
    ast::Exp::RaiseExp(exp) => hir::Exp::Raise(get(cx, exp.exp())),
    ast::Exp::IfExp(exp) => {
      let cond = get(cx, exp.cond());
      let yes = get(cx, exp.yes());
      let no = get(cx, exp.no());
      if_(cx, cond, yes, no)
    }
    ast::Exp::WhileExp(exp) => {
      let vid = cx.fresh();
      let fn_body = {
        let cond = get(cx, exp.cond());
        let body = get(cx, exp.body());
        let call = call_unit_fn(cx, &vid);
        let yes = exp_idx_in_seq(cx, vec![body, call]);
        let yes = cx.arenas.exp.alloc(yes);
        let no = cx.arenas.exp.alloc(tuple([]));
        let fn_body = if_(cx, cond, yes, no);
        cx.arenas.exp.alloc(fn_body)
      };
      let arg_pat = cx.arenas.pat.alloc(pat::tuple([]));
      let fn_exp = cx.arenas.exp.alloc(hir::Exp::Fn(vec![(arg_pat, fn_body)]));
      let vid_pat = cx.arenas.pat.alloc(pat::name(vid.as_str()));
      let val = cx.arenas.dec.alloc(hir::Dec::Val(
        vec![],
        vec![hir::ValBind {
          rec: true,
          pat: vid_pat,
          exp: fn_exp,
        }],
      ));
      hir::Exp::Let(val, call_unit_fn(cx, &vid))
    }
    ast::Exp::CaseExp(exp) => {
      let head = get(cx, exp.exp());
      let arms = matcher(cx, exp.matcher());
      case(cx, head, arms)
    }
    ast::Exp::FnExp(exp) => hir::Exp::Fn(matcher(cx, exp.matcher())),
  };
  Some(ret)
}

pub(crate) fn name(s: &str) -> hir::Exp {
  hir::Exp::Path(hir::Path::one(hir::Name::new(s)))
}

/// TODO do not make 1-tuples
pub(crate) fn tuple<I>(es: I) -> hir::Exp
where
  I: IntoIterator<Item = hir::ExpIdx>,
{
  hir::Exp::Record(
    es.into_iter()
      .enumerate()
      .map(|(idx, e)| (hir::Lab::Num(idx + 1), e))
      .collect(),
  )
}

fn call_unit_fn(cx: &mut Cx, vid: &hir::Name) -> hir::ExpIdx {
  let vid_exp = cx.arenas.exp.alloc(name(vid.as_str()));
  let arg_exp = cx.arenas.exp.alloc(hir::Exp::Record(vec![]));
  cx.arenas.exp.alloc(hir::Exp::App(vid_exp, arg_exp))
}

fn exps_in_seq<I>(cx: &mut Cx, es: I) -> hir::Exp
where
  I: Iterator<Item = ast::ExpInSeq>,
{
  let exps: Vec<_> = es.into_iter().map(|e| get(cx, e.exp())).collect();
  exp_idx_in_seq(cx, exps)
}

/// the Definition says to do the lowering 1 -> 2 but we do the lowering 1 -> 3. all should be
/// equivalent.
///
/// 1. `(e1; ...; en; e)`
/// 2. `case e1 of _ => ... => case en of _ => e`
/// 3. `let val _ = e1 and ... and _ = en in e end`
///
/// the iterator must not be empty, since we need a last expression `e`.
fn exp_idx_in_seq(cx: &mut Cx, mut exps: Vec<hir::ExpIdx>) -> hir::Exp {
  let last = exps.pop().unwrap();
  let dec = hir::Dec::Val(
    vec![],
    exps
      .into_iter()
      .map(|exp| hir::ValBind {
        rec: false,
        pat: cx.arenas.pat.alloc(hir::Pat::Wild),
        exp,
      })
      .collect(),
  );
  hir::Exp::Let(cx.arenas.dec.alloc(dec), last)
}

fn if_(cx: &mut Cx, cond: hir::ExpIdx, yes: hir::ExpIdx, no: hir::ExpIdx) -> hir::Exp {
  let yes_pat = cx.arenas.pat.alloc(pat::name("true"));
  let no_pat = cx.arenas.pat.alloc(pat::name("false"));
  case(cx, cond, vec![(yes_pat, yes), (no_pat, no)])
}

pub(crate) fn case(
  cx: &mut Cx,
  head: hir::ExpIdx,
  arms: Vec<(hir::PatIdx, hir::ExpIdx)>,
) -> hir::Exp {
  hir::Exp::App(cx.arenas.exp.alloc(hir::Exp::Fn(arms)), head)
}

fn matcher(cx: &mut Cx, matcher: Option<ast::Matcher>) -> Vec<(hir::PatIdx, hir::ExpIdx)> {
  matcher
    .into_iter()
    .flat_map(|x| x.match_rules())
    .map(|arm| (pat::get(cx, arm.pat()), get(cx, arm.exp())))
    .collect()
}
