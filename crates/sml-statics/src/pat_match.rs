//! Glue code to talk to [`pattern_match`].

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
        Ty::Con(_, sym) => {
          let all_cons = cons_for_sym(&self.syms, *sym).unwrap_or_else(|| vec![Con::Any]);
          let cur_cons: FxHashSet<_> = cons.collect();
          if all_cons.iter().any(|c| cur_cons.contains(c)) {
            all_cons
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
              self.syms.get_exn(*exn).param.iter().cloned().collect()
            }
            (VariantName::Name(name), Some(sym_info)) => {
              let val_info = sym_info.ty_info.val_env.get(name).ok_or(CheckError)?;
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

const NON_EXN_INFINITE: [Sym; 5] = [Sym::INT, Sym::WORD, Sym::REAL, Sym::CHAR, Sym::STRING];

/// returns the value constructors for the symbol.
///
/// - if `sym` has finite constructors, this returns Some of all of them.
/// - if `sym` has infinite constructors, this returns None.
/// - if `sym` is an opaque type, this returns Some([]). see e.g. `@test(sig::monoid_opaque)`
fn cons_for_sym(syms: &Syms, sym: Sym) -> Option<Vec<Con>> {
  let sym_info = syms.get(sym)?;
  // we just returned None above for exn.
  if NON_EXN_INFINITE.contains(&sym) {
    return None;
  }
  let ret: Vec<_> = sym_info
    .ty_info
    .val_env
    .keys()
    .map(|name| Con::Variant(sym, VariantName::Name(name.clone())))
    .collect();
  Some(ret)
}
