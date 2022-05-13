use crate::common::{get_lab, get_path, get_scon};
use crate::dec;
use crate::util::Cx;
use syntax::ast;

pub(crate) fn get(cx: &mut Cx, exp: Option<ast::Exp>) -> hir::ExpIdx {
  todo!()
}

/// NOTE this should be fine since we ban certain names in identifiers.
fn name_exp(s: &str) -> hir::Exp {
  hir::Exp::Path(hir::Path::new(vec![hir::Name::new(s)]))
}

fn tuple<I>(es: I) -> hir::Exp
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

fn get_(cx: &mut Cx, exp: ast::Exp) -> Option<hir::Exp> {
  let ret = match exp {
    ast::Exp::SConExp(exp) => hir::Exp::SCon(get_scon(exp.s_con()?)?),
    ast::Exp::PathExp(exp) => hir::Exp::Path(get_path(exp.path()?)?),
    ast::Exp::RecordExp(exp) => hir::Exp::Record(
      exp
        .exp_rows()
        .filter_map(|x| {
          let l = get_lab(x.lab()?)?;
          let e = get(cx, x.exp());
          Some((l, e))
        })
        .collect(),
    ),
    ast::Exp::SelectorExp(exp) => {
      let lab = get_lab(exp.lab()?)?;
      let name = cx.fresh();
      let var = hir::Pat::Con(name.clone().into(), None);
      let var = cx.arenas.pat.alloc(var);
      let param = hir::Pat::Record {
        pats: vec![(lab, var)],
        allows_other: true,
      };
      let param = cx.arenas.pat.alloc(param);
      let body = hir::Exp::Path(name.into());
      let body = cx.arenas.exp.alloc(body);
      hir::Exp::Fn(vec![(param, body)])
    }
    ast::Exp::TupleExp(exp) => tuple(exp.exp_args().map(|e| get(cx, e.exp()))),
    ast::Exp::ListExp(exp) => {
      // need to rev()
      #[allow(clippy::needless_collect)]
      let exps: Vec<_> = exp.exp_args().map(|e| get(cx, e.exp())).collect();
      exps.into_iter().rev().fold(name_exp("nil"), |ac, x| {
        let cons = cx.arenas.exp.alloc(name_exp("::"));
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
    ast::Exp::AppExp(_) => todo!(),
    ast::Exp::InfixExp(_) => todo!(),
    ast::Exp::TypedExp(_) => todo!(),
    ast::Exp::AndalsoExp(_) => todo!(),
    ast::Exp::OrelseExp(_) => todo!(),
    ast::Exp::HandleExp(_) => todo!(),
    ast::Exp::RaiseExp(_) => todo!(),
    ast::Exp::IfExp(_) => todo!(),
    ast::Exp::WhileExp(_) => todo!(),
    ast::Exp::CaseExp(_) => todo!(),
    ast::Exp::FnExp(_) => todo!(),
  };
  Some(ret)
}

/// the Definition says to do the lowering 1 -> 2 but we do the lowering 1 -> 3. all should be
/// equivalent.
///
/// 1. `(e1; ...; en; e)`
/// 2. `case e1 of _ => ... => case en of _ => e`
/// 3. `let val _ = e1 and ... and _ = en in e end`
///
/// the iterator must not be empty, since we need a last expression `e`.
fn exps_in_seq<I>(cx: &mut Cx, es: I) -> hir::Exp
where
  I: Iterator<Item = ast::ExpInSeq>,
{
  let mut exps: Vec<_> = es.map(|e| get(cx, e.exp())).collect();
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
