use crate::types::{Exn, Sym, Syms, Ty};
use crate::util::apply_bv;
use fast_hash::FxHashSet;
use pattern_match::{CheckError, Result};
use std::collections::BTreeSet;

pub(crate) type Pat = pattern_match::Pat<Lang>;

pub(crate) struct Lang {
  pub(crate) syms: Syms,
}

impl pattern_match::Lang for Lang {
  type PatIdx = sml_hir::PatIdx;

  type Con = Con;

  type Ty = Ty;

  fn any(&self) -> Self::Con {
    Con::Any
  }

  fn split<'a, I>(&self, ty: &Self::Ty, con: &Self::Con, cons: I) -> Result<Vec<Self::Con>>
  where
    Self::Con: 'a,
    I: Iterator<Item = &'a Self::Con>,
  {
    let ret = match con {
      Con::Any => match ty {
        Ty::None | Ty::BoundVar(_) | Ty::MetaVar(_) | Ty::FixedVar(_) | Ty::Fn(_, _) => {
          vec![Con::Any]
        }
        Ty::Con(_, ty_name) => {
          let ty_info = match self.syms.get(*ty_name) {
            // we can't know how many variants of exn there are, since it's EXteNsible.
            None => return Ok(vec![Con::Any]),
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
        Ty::Record(fs) => {
          vec![Con::Record { labels: fs.keys().cloned().collect(), allows_other: false }]
        }
      },
      Con::Int(_)
      | Con::Word(_)
      | Con::Char(_)
      | Con::String(_)
      | Con::Record { .. }
      | Con::Variant(_, _) => {
        vec![con.clone()]
      }
    };
    Ok(ret)
  }

  fn get_arg_tys(&self, ty: &Self::Ty, con: &Self::Con) -> Result<Vec<Self::Ty>> {
    let ret = match ty {
      Ty::None | Ty::BoundVar(_) | Ty::MetaVar(_) | Ty::FixedVar(_) | Ty::Fn(_, _) => Vec::new(),
      Ty::Record(rows) => rows.iter().map(|(_, t)| t.clone()).collect(),
      Ty::Con(args, ty_name) => match con {
        Con::Any | Con::Int(_) | Con::Word(_) | Con::Char(_) | Con::String(_) => Vec::new(),
        Con::Variant(ty_name_2, variant_name) => {
          if ty_name != ty_name_2 {
            return Err(CheckError);
          }
          match (variant_name, self.syms.get(*ty_name)) {
            (VariantName::Exn(exn), None) => {
              self.syms.get_exn(*exn).1.into_iter().cloned().collect()
            }
            (VariantName::Name(name), Some((_, ty_info))) => {
              let val_info = ty_info.val_env.get(name).ok_or(CheckError)?;
              match &val_info.ty_scheme.ty {
                Ty::Con(_, _) => Vec::new(),
                Ty::Fn(arg, _) => {
                  let mut arg = arg.as_ref().clone();
                  apply_bv(args, &mut arg);
                  vec![arg]
                }
                _ => return Err(CheckError),
              }
            }
            _ => return Err(CheckError),
          }
        }
        Con::Record { .. } => return Err(CheckError),
      },
    };
    Ok(ret)
  }

  fn covers(&self, lhs: &Self::Con, rhs: &Self::Con) -> bool {
    matches!((lhs, rhs), (Con::Record { allows_other: true, .. }, Con::Record { .. },))
      || (lhs == rhs)
  }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub(crate) enum Con {
  Any,
  Int(sml_hir::Int),
  Word(u64),
  Char(char),
  String(str_util::SmolStr),
  Record { labels: BTreeSet<sml_hir::Lab>, allows_other: bool },
  Variant(Sym, VariantName),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub(crate) enum VariantName {
  Name(str_util::Name),
  Exn(Exn),
}
