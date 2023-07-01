//! Glue code to talk to [`pattern_match`].

use fast_hash::FxHashSet;
use pattern_match::{CheckError, Result};
use sml_statics_types::sym::{Exn, Sym, Syms};
use sml_statics_types::ty::{Ty, TyData, Tys};
use sml_statics_types::util::apply_bv;
use std::collections::BTreeSet;

pub(crate) type Pat = pattern_match::Pat<Lang>;

pub(crate) struct Lang;

pub(crate) struct Cx<'a> {
  pub(crate) syms: &'a Syms,
  pub(crate) tys: &'a mut Tys,
}

impl pattern_match::Lang for Lang {
  type Cx<'a> = Cx<'a>;

  type PatIdx = sml_hir::PatIdx;

  type Con = Con;

  type Ty = Ty;

  fn any() -> Con {
    Con::Any
  }

  fn split<'a, I>(cx: &mut Cx<'_>, ty: &Ty, con: &Con, cons: I, depth: usize) -> Result<Vec<Con>>
  where
    Con: 'a,
    I: Iterator<Item = &'a Con>,
  {
    let ret = match con {
      Con::Any => match cx.tys.data(*ty) {
        TyData::None
        | TyData::BoundVar(_)
        | TyData::UnsolvedMetaVar(_)
        | TyData::GeneralizedMetaVar(_)
        | TyData::FixedVar(_)
        | TyData::Fn(_) => {
          vec![Con::Any]
        }
        TyData::Con(data) => {
          let all_cons = cons_for_sym(cx.syms, data.sym).unwrap_or_else(|| vec![Con::Any]);
          let cur_cons: FxHashSet<_> = cons.collect();
          // this is... a little strange.
          //
          // the depth chosen here is somewhat arbitrary. it guards against us recursing too far and
          // generating too many witnesses. if the depth is small, we choose a method that will
          // allow us to use all_cons more (and therefore generate better witnesses), but if not, we
          // start being more conservative and use all_cons less.
          //
          // although we call it "depth", it's not even really the nested-ness of the patterns, but
          // rather how many calls deep in the pattern matching checking code we are. so for
          // instance @test(matching::list_missing_len_3) will fail if you tweak this threshold on
          // the RHS down a little, then, if you tweak it down more, the similar
          // @test(matching::list_missing_len_1) will fail too.
          //
          // see also @test(matching::parser). were it not for this depth check we would be
          // generating tens of thousands of witnesses.
          let use_any = if depth < 10 {
            all_cons.iter().all(|c| !cur_cons.contains(c))
          } else {
            all_cons.is_empty()
              || cur_cons.iter().any(|c| **c == Con::Any)
              || all_cons.iter().any(|c| !cur_cons.contains(c))
          };
          if use_any {
            vec![Con::Any]
          } else {
            all_cons
          }
        }
        TyData::Record(rows) => {
          vec![Con::Record { labels: rows.keys().cloned().collect(), allows_other: false }]
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

  fn get_arg_tys(cx: &mut Cx<'_>, ty: &Ty, con: &Con) -> Result<Vec<Ty>> {
    let ret = match cx.tys.data(*ty) {
      TyData::None
      | TyData::BoundVar(_)
      | TyData::UnsolvedMetaVar(_)
      | TyData::GeneralizedMetaVar(_)
      | TyData::FixedVar(_)
      | TyData::Fn(_) => Vec::new(),
      TyData::Record(rows) => rows.values().copied().collect(),
      TyData::Con(data) => match con {
        Con::Any | Con::Int(_) | Con::Word(_) | Con::Char(_) | Con::String(_) => Vec::new(),
        Con::Variant(sym, variant_name) => {
          if data.sym != *sym {
            return Err(CheckError);
          }
          match (variant_name, cx.syms.get(data.sym)) {
            (VariantName::Exn(exn), None) => cx.syms.get_exn(*exn).param.iter().copied().collect(),
            (VariantName::Name(name), Some(sym_info)) => {
              let val_info = sym_info.ty_info.val_env.get(name).ok_or(CheckError)?;
              match cx.tys.data(val_info.ty_scheme.ty) {
                TyData::Con(_) => Vec::new(),
                TyData::Fn(fn_data) => {
                  let mut param = fn_data.param;
                  apply_bv(cx.tys, &data.args, &mut param);
                  vec![param]
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

  fn covers(lhs: &Con, rhs: &Con) -> bool {
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
    .iter()
    .map(|(name, _)| Con::Variant(sym, VariantName::Name(name.clone())))
    .collect();
  Some(ret)
}
