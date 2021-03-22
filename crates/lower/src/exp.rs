use crate::common::{lab, path, scon};
use crate::util::Cx;
use syntax::ast::Exp;

pub(crate) fn get(cx: &mut Cx, exp: Exp) -> hir::ExpIdx {
  todo!()
}

pub(crate) fn _get(cx: &mut Cx, exp: Exp) -> Option<hir::Exp> {
  let ret = match exp {
    Exp::SConExp(exp) => hir::Exp::SCon(scon(exp.s_con()?.kind)),
    Exp::PathExp(exp) => hir::Exp::Path(path(exp.path()?)?),
    Exp::RecordExp(exp) => hir::Exp::Record(
      exp
        .exp_rows()
        .map(|x| {
          let l = lab(x.lab()?)?;
          let e = get(cx, x.exp()?);
          Some((l, e))
        })
        .collect::<Option<_>>()?,
    ),
    Exp::SelectorExp(_) => todo!(),
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
