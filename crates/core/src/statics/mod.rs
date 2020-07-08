//! Statics.
//!
//! With help from [this article][1].
//!
//! [1]: http://dev.stephendiehl.com/fun/006_hindley_milner.html

mod ck;
mod std_lib;
mod types;

use crate::ast::TopDec;
use crate::intern::StrRef;
use crate::loc::Located;
use crate::statics::types::{Error, Result, Ty};

pub fn get(top_decs: &[Located<TopDec<StrRef>>]) -> Result<()> {
  let (mut bs, mut st) = std_lib::get();
  for top_dec in top_decs {
    ck::ck_top_dec(&mut bs, &mut st, top_dec)?;
  }
  'outer: for (loc, tv, overloads) in st.overload {
    for name in overloads {
      let mut pre = st.subst.clone();
      if let Ok(()) = pre.unify(loc, Ty::Var(tv), Ty::base(name)) {
        st.subst = pre;
        continue 'outer;
      }
    }
    return Err(loc.wrap(Error::NoSuitableOverload));
  }
  bs.apply(&st.subst, &mut st.datatypes);
  assert!(bs.free_ty_vars(&st.datatypes).is_empty());
  Ok(())
}
