use crate::types::{Sym, Syms, Ty};
use rustc_hash::FxHashSet;

pub(crate) type Pat = pattern_match::Pat<Lang>;

pub(crate) struct Lang {
  pub(crate) syms: Syms,
}

impl pattern_match::Lang for Lang {
  type PatIdx = hir::PatIdx;

  type Con = Con;

  type Ty = Ty;

  fn any(&self) -> Self::Con {
    Con::Any
  }

  fn split<'a, I>(&self, ty: &Self::Ty, con: &Self::Con, cons: I) -> Vec<Self::Con>
  where
    Self::Con: 'a,
    I: Iterator<Item = &'a Self::Con>,
  {
    match con {
      Con::Any => match ty {
        Ty::None | Ty::BoundVar(_) | Ty::MetaVar(_) | Ty::Fn(_, _) => {
          vec![Con::Any]
        }
        Ty::Con(_, ty_name) => {
          let ty_info = self.syms.get(ty_name);
          let all: Vec<_> = ty_info
            .val_env
            .keys()
            .map(|con_name| Con::Variant(*ty_name, con_name.clone()))
            .collect();
          let set: FxHashSet<_> = cons.collect();
          if all.iter().any(|c| set.contains(c)) {
            all
          } else {
            vec![Con::Any]
          }
        }
        Ty::Record(fs) => vec![Con::Record(fs.keys().cloned().collect())],
      },
      Con::Int(_)
      | Con::Word(_)
      | Con::Char(_)
      | Con::String(_)
      | Con::Record(_)
      | Con::Variant(_, _) => {
        vec![con.clone()]
      }
    }
  }

  fn get_arg_tys(&self, ty: &Self::Ty, con: &Self::Con) -> Vec<Self::Ty> {
    match ty {
      Ty::None | Ty::BoundVar(_) | Ty::MetaVar(_) | Ty::Fn(_, _) => Vec::new(),
      Ty::Record(rows) => rows.iter().map(|(_, t)| t.clone()).collect(),
      Ty::Con(args, ty_name) => match con {
        Con::Any => Vec::new(),
        Con::Variant(ty_name_2, con_name) => {
          assert_eq!(ty_name, ty_name_2);
          let ty_info = self.syms.get(ty_name);
          let _ = args;
          let _ = ty_info.val_env.get(con_name).unwrap().ty_scheme.ty;
          todo!()
        }
        _ => unreachable!(),
      },
    }
  }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub(crate) enum Con {
  Any,
  Int(i32),
  Word(i32),
  Char(u8),
  String(hir::SmolStr),
  Record(Vec<hir::Lab>),
  Variant(Sym, hir::Name),
}
