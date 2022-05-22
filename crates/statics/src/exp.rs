use crate::cx::Cx;
use crate::ty;
use crate::types::{Sym, Ty};
use crate::unify::unify;
use crate::util::{apply, get_scon, record};

pub(crate) fn get(cx: &mut Cx, ars: &hir::Arenas, exp: hir::ExpIdx) -> Ty {
  match ars.exp[exp] {
    hir::Exp::None => Ty::None,
    hir::Exp::SCon(ref scon) => get_scon(scon),
    hir::Exp::Path(_) => todo!(),
    hir::Exp::Record(ref rows) => record(cx, rows, |cx, exp| get(cx, ars, exp)),
    hir::Exp::Let(_, _) => todo!(),
    hir::Exp::App(func, arg) => {
      let want = get(cx, ars, func);
      let arg_ty = get(cx, ars, arg);
      let mut res_ty = Ty::MetaVar(cx.gen_meta_var());
      let got = Ty::Fn(arg_ty.into(), res_ty.clone().into());
      unify(cx, want, got);
      apply(cx.subst(), &mut res_ty);
      res_ty
    }
    hir::Exp::Handle(_, _) => todo!(),
    hir::Exp::Raise(exp) => {
      let got = get(cx, ars, exp);
      unify(cx, Ty::zero(Sym::EXN), got);
      Ty::MetaVar(cx.gen_meta_var())
    }
    hir::Exp::Fn(_) => todo!(),
    hir::Exp::Typed(exp, want) => {
      let got = get(cx, ars, exp);
      let mut want = ty::get(cx, ars, want);
      unify(cx, want.clone(), got);
      apply(cx.subst(), &mut want);
      want
    }
  }
}
