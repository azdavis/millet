use crate::types::{Exn, Sym, Syms, Ty};
use crate::util::apply_bv;
use fast_hash::FxHashSet;

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
        Ty::None | Ty::BoundVar(_) | Ty::MetaVar(_) | Ty::FixedVar(_) | Ty::Fn(_, _) => {
          vec![Con::Any]
        }
        Ty::Con(_, ty_name) => {
          let ty_info = match self.syms.get(ty_name) {
            // we can't know how many variants of exn there are, since it's EXteNsible.
            None => return vec![Con::Any],
            Some((_, x)) => x,
          };
          let all: Vec<_> = ty_info
            .val_env
            .keys()
            .map(|name| Con::Variant(*ty_name, VariantName::Name(name.clone())))
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
      Ty::None | Ty::BoundVar(_) | Ty::MetaVar(_) | Ty::FixedVar(_) | Ty::Fn(_, _) => Vec::new(),
      Ty::Record(rows) => rows.iter().map(|(_, t)| t.clone()).collect(),
      Ty::Con(args, ty_name) => match con {
        Con::Any | Con::Int(_) | Con::Word(_) | Con::Char(_) | Con::String(_) => Vec::new(),
        Con::Variant(ty_name_2, variant_name) => {
          assert_eq!(ty_name, ty_name_2);
          match (variant_name, self.syms.get(ty_name)) {
            (VariantName::Exn(exn), None) => {
              self.syms.get_exn(exn).1.into_iter().cloned().collect()
            }
            (VariantName::Name(name), Some((_, ty_info))) => {
              match &ty_info.val_env.get(name).unwrap().ty_scheme.ty {
                Ty::Con(_, _) => Vec::new(),
                Ty::Fn(arg, _) => {
                  let mut arg = arg.as_ref().clone();
                  apply_bv(args, &mut arg);
                  vec![arg]
                }
                _ => unreachable!(),
              }
            }
            _ => unreachable!(),
          }
        }
        // NOTE: will be unreachable in a well-typed match, but we can reach this in a
        // not-well-typed match. see `misc::match_record_non_record_ty`.
        Con::Record(labs) => labs.iter().map(|_| Ty::None).collect(),
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
  Variant(Sym, VariantName),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub(crate) enum VariantName {
  Name(hir::Name),
  Exn(Exn),
}
