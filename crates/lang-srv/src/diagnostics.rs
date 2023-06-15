//! Publish diagnostics.

use crate::convert;
use crate::state::{Mode, St};
use fast_hash::{FxHashMap, FxHashSet};
use lsp_types::Url;
use paths::FileSystem as _;

pub(crate) fn try_publish(st: &mut St) -> bool {
  let root = match &mut st.mode {
    Mode::Root(x) => x,
    Mode::NoRoot => return false,
  };
  let mut input_diagnostics = FxHashMap::<Url, Vec<lsp_types::Diagnostic>>::default();
  for err in &root.input.errors {
    let did_send_as_diagnostic = if st.cx.fs.is_file(err.abs_path()) {
      match convert::file_url(err.abs_path()) {
        Ok(url) => {
          let d = convert::diagnostic(
            err.display(root.path.as_path()).to_string(),
            err.range(),
            err.code(),
            err.severity(),
            st.cx.options.diagnostics.more_info_hint.0,
          );
          input_diagnostics.entry(url).or_default().push(d);
          true
        }
        Err(e) => {
          log::error!("couldn't get path as a file url: {e:#}");
          false
        }
      }
    } else {
      false
    };
    if !did_send_as_diagnostic {
      st.cx.show_error(
        format!(
          "{}: {}",
          err.maybe_rel_path(root.path.as_path()).display(),
          err.display(root.path.as_path())
        ),
        err.code(),
      );
    }
  }
  let mut has_diagnostics = FxHashSet::<Url>::default();
  for (url, ds) in input_diagnostics {
    has_diagnostics.insert(url.clone());
    st.cx.send_diagnostics(url, ds);
  }
  let got_many = st.analysis.get_many(&root.input);
  for (path_id, errors) in got_many {
    let path = st.cx.paths.get_path(path_id);
    let url = match convert::file_url(path.as_path()) {
      Ok(x) => x,
      Err(e) => {
        log::error!("couldn't get path as a file url: {e:#}");
        continue;
      }
    };
    let ds = convert::diagnostics(errors, st.cx.options.diagnostics.more_info_hint.0);
    if ds.is_empty() {
      continue;
    }
    has_diagnostics.insert(url.clone());
    st.cx.send_diagnostics(url, ds);
  }
  // iter over the old list of urls with diagnostics.
  for url in std::mem::take(&mut st.has_diagnostics) {
    if has_diagnostics.contains(&url) {
      // had old and new diagnostics. just sent the new ones.
      continue;
    }
    // had old diagnostics, but no new diagnostics. clear the old diagnostics.
    st.cx.send_diagnostics(url, Vec::new());
  }
  st.has_diagnostics = has_diagnostics;
  true
}
