use crate::common::{get_lab, get_path, get_scon};
use crate::util::Cx;
use syntax::ast::Exp;

pub(crate) fn get(cx: &mut Cx, exp: Exp) -> hir::ExpIdx {
  todo!()
}

fn get_(cx: &mut Cx, exp: Exp) -> Option<hir::Exp> {
  let ret = match exp {
    Exp::SConExp(exp) => hir::Exp::SCon(get_scon(exp.s_con()?)?),
    Exp::PathExp(exp) => hir::Exp::Path(get_path(exp.path()?)?),
    Exp::RecordExp(exp) => hir::Exp::Record(
      exp
        .exp_rows()
        .map(|x| {
          let l = get_lab(x.lab()?)?;
          let e = get(cx, x.exp()?);
          Some((l, e))
        })
        .collect::<Option<_>>()?,
    ),
    Exp::SelectorExp(exp) => {
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
    Exp::TupleExp(_) => todo!(),
    Exp::ListExp(_) => todo!(),
    Exp::SeqExp(_) => todo!(),
    Exp::LetExp(_) => todo!(),
    Exp::AppExp(_) => todo!(),
    Exp::InfixExp(_) => todo!(),
    Exp::TypedExp(_) => todo!(),
    Exp::AndalsoExp(_) => todo!(),
    Exp::OrelseExp(_) => todo!(),
    Exp::HandleExp(_) => todo!(),
    Exp::RaiseExp(_) => todo!(),
    Exp::IfExp(_) => todo!(),
    Exp::WhileExp(_) => todo!(),
    Exp::CaseExp(_) => todo!(),
    Exp::FnExp(_) => todo!(),
  };
  Some(ret)
}
