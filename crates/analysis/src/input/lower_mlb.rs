//! Lower a MLB syntax `BasDec` into the HIR equivalent.

use crate::input::util::{
  get_path_id, read_file, Error, ErrorKind, ErrorSource, GroupPathToProcess, Result,
};
use fast_hash::FxHashSet;
use paths::{PathId, PathMap};
use std::path::Path;

pub(crate) struct MlbCx<'a, F> {
  pub(crate) path: &'a Path,
  pub(crate) parent: &'a Path,
  pub(crate) pos_db: &'a text_pos::PositionDb,
  pub(crate) fs: &'a F,
  pub(crate) store: &'a mut paths::Store,
  pub(crate) sources: &'a mut PathMap<String>,
  pub(crate) stack: &'a mut Vec<GroupPathToProcess>,
  pub(crate) path_id: PathId,
}

pub(crate) fn get_bas_dec<F>(
  cx: &mut MlbCx<'_, F>,
  dec: mlb_syntax::BasDec,
) -> Result<mlb_statics::BasDec>
where
  F: paths::FileSystem,
{
  let ret = match dec {
    mlb_syntax::BasDec::Basis(binds) => {
      let mut names = FxHashSet::<str_util::Name>::default();
      let binds = binds
        .into_iter()
        .map(|(name, exp)| {
          if !names.insert(name.val.clone()) {
            return Err(Error {
              source: ErrorSource { path: None, range: cx.pos_db.range(name.range) },
              path: cx.path.to_owned(),
              kind: ErrorKind::Duplicate(name.val),
            });
          }
          let exp = get_bas_exp(cx, exp)?;
          Ok(mlb_statics::BasDec::Basis(name, exp.into()))
        })
        .collect::<Result<Vec<_>>>()?;
      mlb_statics::BasDec::seq(binds)
    }
    mlb_syntax::BasDec::Open(names) => {
      mlb_statics::BasDec::seq(names.into_iter().map(mlb_statics::BasDec::Open).collect())
    }
    mlb_syntax::BasDec::Local(local_dec, in_dec) => mlb_statics::BasDec::Local(
      get_bas_dec(cx, *local_dec)?.into(),
      get_bas_dec(cx, *in_dec)?.into(),
    ),
    mlb_syntax::BasDec::Export(ns, binds) => {
      let mut names = FxHashSet::<str_util::Name>::default();
      let binds = binds
        .into_iter()
        .map(|(lhs, rhs)| {
          if !names.insert(lhs.val.clone()) {
            return Err(Error {
              source: ErrorSource { path: None, range: cx.pos_db.range(lhs.range) },
              path: cx.path.to_owned(),
              kind: ErrorKind::Duplicate(lhs.val),
            });
          }
          let rhs = rhs.unwrap_or_else(|| lhs.clone());
          let ns = match ns {
            mlb_syntax::Namespace::Structure => mlb_statics::Namespace::Structure,
            mlb_syntax::Namespace::Signature => mlb_statics::Namespace::Signature,
            mlb_syntax::Namespace::Functor => mlb_statics::Namespace::Functor,
          };
          Ok(mlb_statics::BasDec::Export(ns, lhs, rhs))
        })
        .collect::<Result<Vec<_>>>()?;
      mlb_statics::BasDec::seq(binds)
    }
    mlb_syntax::BasDec::Path(parsed_path) => {
      let source =
        ErrorSource { path: Some(cx.path.to_owned()), range: cx.pos_db.range(parsed_path.range) };
      let path = cx.parent.join(parsed_path.val.as_path());
      let path_id = get_path_id(cx.fs, cx.store, source.clone(), path.as_path())?;
      let kind = match parsed_path.val.kind() {
        mlb_syntax::PathKind::Sml => {
          let contents = read_file(cx.fs, source, path.as_path())?;
          cx.sources.insert(path_id, contents);
          mlb_statics::PathKind::Sml
        }
        mlb_syntax::PathKind::Mlb => {
          cx.stack.push(GroupPathToProcess {
            parent: cx.path_id,
            range: source.range,
            path: path_id,
          });
          mlb_statics::PathKind::Mlb
        }
      };
      mlb_statics::BasDec::Path(path_id, kind)
    }
    mlb_syntax::BasDec::Ann(_, dec) => get_bas_dec(cx, *dec)?,
    mlb_syntax::BasDec::Seq(decs) => mlb_statics::BasDec::seq(
      decs.into_iter().map(|dec| get_bas_dec(cx, dec)).collect::<Result<Vec<_>>>()?,
    ),
  };
  Ok(ret)
}

fn get_bas_exp<F>(cx: &mut MlbCx<'_, F>, exp: mlb_syntax::BasExp) -> Result<mlb_statics::BasExp>
where
  F: paths::FileSystem,
{
  let ret = match exp {
    mlb_syntax::BasExp::Bas(dec) => mlb_statics::BasExp::Bas(get_bas_dec(cx, dec)?),
    mlb_syntax::BasExp::Name(name) => mlb_statics::BasExp::Name(name),
    mlb_syntax::BasExp::Let(dec, exp) => {
      mlb_statics::BasExp::Let(get_bas_dec(cx, dec)?, get_bas_exp(cx, *exp)?.into())
    }
  };
  Ok(ret)
}
