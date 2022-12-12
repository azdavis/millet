//! Lower a MLB root group into a map of source files and parsed groups.

use crate::types::Group;
use crate::util::{
  get_path_id_in_group, read_file, Error, ErrorKind, ErrorSource, GroupPathToProcess, Result,
  StartedGroup,
};
use fast_hash::FxHashSet;
use paths::{PathId, PathMap};

pub(crate) fn get<F>(
  fs: &F,
  sources: &mut PathMap<String>,
  groups: &mut PathMap<Group>,
  store: &mut paths::Store,
  path_vars: &paths::slash_var_path::Env,
  path: paths::PathId,
) -> Result<()>
where
  F: paths::FileSystem,
{
  let init = GroupPathToProcess { parent: path, range: None, path };
  let mut st = St { fs, store, sources, stack: vec![init] };
  while let Some(cur) = st.stack.pop() {
    if groups.contains_key(&cur.path) {
      continue;
    }
    let group = StartedGroup::new(st.store, cur, fs)?;
    let syntax_dec = match mlb_syntax::get(group.contents.as_str(), path_vars) {
      Ok(x) => x,
      Err(e) => {
        return Err(Error::new(
          ErrorSource { path: None, range: group.pos_db.range(e.text_range()) },
          group.path.as_path().to_owned(),
          ErrorKind::Mlb(e),
        ));
      }
    };
    let cx = Cx { group, path_id: cur.path };
    let bas_dec = get_bas_dec(&mut st, &cx, syntax_dec)?;
    groups.insert(cur.path, Group { bas_dec, pos_db: cx.group.pos_db });
  }
  Ok(())
}

struct St<'a, F> {
  fs: &'a F,
  store: &'a mut paths::Store,
  sources: &'a mut PathMap<String>,
  stack: Vec<GroupPathToProcess>,
}

struct Cx {
  group: StartedGroup,
  path_id: PathId,
}

fn get_bas_dec<F>(
  st: &mut St<'_, F>,
  cx: &Cx,
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
            return Err(Error::new(
              ErrorSource { path: None, range: cx.group.pos_db.range(name.range) },
              cx.group.path.as_path().to_owned(),
              ErrorKind::Duplicate(name.val),
            ));
          }
          let exp = get_bas_exp(st, cx, exp)?;
          Ok(mlb_statics::BasDec::Basis(name, exp.into()))
        })
        .collect::<Result<Vec<_>>>()?;
      mlb_statics::BasDec::seq(binds)
    }
    mlb_syntax::BasDec::Open(names) => {
      mlb_statics::BasDec::seq(names.into_iter().map(mlb_statics::BasDec::Open).collect())
    }
    mlb_syntax::BasDec::Local(local_dec, in_dec) => mlb_statics::BasDec::Local(
      get_bas_dec(st, cx, *local_dec)?.into(),
      get_bas_dec(st, cx, *in_dec)?.into(),
    ),
    mlb_syntax::BasDec::Export(ns, binds) => {
      let mut names = FxHashSet::<str_util::Name>::default();
      let binds = binds
        .into_iter()
        .map(|(lhs, rhs)| {
          if !names.insert(lhs.val.clone()) {
            return Err(Error::new(
              ErrorSource { path: None, range: cx.group.pos_db.range(lhs.range) },
              cx.group.path.as_path().to_owned(),
              ErrorKind::Duplicate(lhs.val),
            ));
          }
          let rhs = rhs.unwrap_or_else(|| lhs.clone());
          let ns = match ns {
            mlb_syntax::Namespace::Structure => sml_statics::basis::Namespace::Structure,
            mlb_syntax::Namespace::Signature => sml_statics::basis::Namespace::Signature,
            mlb_syntax::Namespace::Functor => sml_statics::basis::Namespace::Functor,
          };
          Ok(mlb_statics::BasDec::Export(ns, lhs, rhs))
        })
        .collect::<Result<Vec<_>>>()?;
      mlb_statics::BasDec::seq(binds)
    }
    mlb_syntax::BasDec::Path(pp) => {
      let (path_id, path, source) =
        get_path_id_in_group(st.fs, st.store, &cx.group, pp.val.as_path(), pp.range)?;
      let kind = match pp.val.kind() {
        mlb_syntax::PathKind::Sml => {
          let contents = read_file(st.fs, source, path.as_path())?;
          st.sources.insert(path_id, contents);
          mlb_statics::PathKind::Source
        }
        mlb_syntax::PathKind::Mlb => {
          st.stack.push(GroupPathToProcess {
            parent: cx.path_id,
            range: source.range,
            path: path_id,
          });
          mlb_statics::PathKind::Group
        }
      };
      mlb_statics::BasDec::Path(path_id, kind)
    }
    mlb_syntax::BasDec::Ann(_, dec) => get_bas_dec(st, cx, *dec)?,
    mlb_syntax::BasDec::Seq(decs) => mlb_statics::BasDec::seq(
      decs.into_iter().map(|dec| get_bas_dec(st, cx, dec)).collect::<Result<Vec<_>>>()?,
    ),
  };
  Ok(ret)
}

fn get_bas_exp<F>(
  st: &mut St<'_, F>,
  cx: &Cx,
  exp: mlb_syntax::BasExp,
) -> Result<mlb_statics::BasExp>
where
  F: paths::FileSystem,
{
  let ret = match exp {
    mlb_syntax::BasExp::Bas(dec) => mlb_statics::BasExp::Bas(get_bas_dec(st, cx, dec)?),
    mlb_syntax::BasExp::Name(name) => mlb_statics::BasExp::Name(name),
    mlb_syntax::BasExp::Let(dec, exp) => {
      mlb_statics::BasExp::Let(get_bas_dec(st, cx, dec)?, get_bas_exp(st, cx, *exp)?.into())
    }
  };
  Ok(ret)
}
