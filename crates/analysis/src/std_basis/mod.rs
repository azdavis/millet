use once_cell::sync::Lazy;
use statics::Statics;

/// What standard basis to use.
#[derive(Debug, Clone, Copy)]
pub enum StdBasis {
  /// Include most of the standard basis library.
  Full,
  /// Only include fundamental top-level definitions like `int`, `real`, `ref`, `<`, etc.
  Minimal,
}

impl StdBasis {
  pub(crate) fn into_statics(self) -> Statics {
    match self {
      StdBasis::Full => FULL.clone(),
      StdBasis::Minimal => Statics::default(),
    }
  }
}

fn get_full_std_basis() -> Statics {
  let mut st = Statics::default();
  let mode = statics::Mode::Declaration;
  for &contents in ORDER {
    let lexed = lex::get(contents);
    if let Some(e) = lexed.errors.first() {
      panic!("std_basis error: lex: {}", e.display());
    }
    let parsed = parse::get(&lexed.tokens);
    if let Some(e) = parsed.errors.first() {
      panic!("std_basis error: parse: {}", e.display());
    }
    let mut lowered = lower::get(&parsed.root);
    if let Some(e) = lowered.errors.first() {
      panic!("std_basis error: lower: {}", e.display());
    }
    ty_var_scope::get(&mut lowered.arenas, &lowered.top_decs);
    let (_, es) = statics::get(&mut st, mode, &lowered.arenas, &lowered.top_decs);
    if let Some(e) = es.first() {
      panic!("std_basis error: statics: {}", e.display(&st.syms));
    }
  }
  st
}

static FULL: Lazy<Statics> = Lazy::new(|| elapsed::log("get_full_std_basis", get_full_std_basis));

const ORDER: &[&str] = &[
  include_str!("general.sml"),
  include_str!("option.sml"),
  include_str!("list.sml"),
  include_str!("list-pair.sml"),
  include_str!("string-cvt.sml"),
  include_str!("integer.sml"),
  include_str!("bool.sml"),
  include_str!("vector.sml"),
  include_str!("vector-slice.sml"),
  include_str!("array.sml"),
  include_str!("array-slice.sml"),
  include_str!("array2.sml"),
  include_str!("command-line.sml"),
  include_str!("ieee-float.sml"),
  include_str!("io.sml"),
  include_str!("math.sml"),
  include_str!("real.sml"),
  include_str!("word.sml"),
  include_str!("string.sml"),
  include_str!("char.sml"),
  include_str!("substring.sml"),
  include_str!("mono-vector.sml"),
  include_str!("mono-vector-slice.sml"),
  include_str!("mono-array.sml"),
  include_str!("mono-array-slice.sml"),
  include_str!("mono-array2.sml"),
  include_str!("int-inf.sml"),
  include_str!("byte.sml"),
  include_str!("stream-io.sml"),
  include_str!("time.sml"),
  include_str!("date.sml"),
  include_str!("timer.sml"),
  include_str!("os-file-sys.sml"),
  include_str!("os-io.sml"),
  include_str!("os-path.sml"),
  include_str!("os-process.sml"),
  include_str!("os.sml"),
  include_str!("prim-io.sml"),
  include_str!("imperative-io.sml"),
  include_str!("bin-io.sml"),
  include_str!("bit-flags.sml"),
  include_str!("pack-float.sml"),
  include_str!("pack-word.sml"),
  include_str!("net-host-db.sml"),
  include_str!("socket.sml"),
  include_str!("generic-sock.sml"),
  include_str!("inet-sock.sml"),
  include_str!("posix-error.sml"),
  include_str!("posix-file-sys.sml"),
  include_str!("posix-io.sml"),
  include_str!("posix-proc-env.sml"),
  include_str!("posix-process.sml"),
  include_str!("posix-signal.sml"),
  include_str!("posix-sys-db.sml"),
  include_str!("posix-tty.sml"),
  include_str!("posix.sml"),
  include_str!("prot-db.sml"),
  include_str!("serv-db.sml"),
  include_str!("text-stream-io.sml"),
  include_str!("text-io.sml"),
  include_str!("text.sml"),
  include_str!("unix-sock.sml"),
  include_str!("unix.sml"),
  include_str!("windows.sml"),
  // non-standard
  include_str!("fn.sml"),
  include_str!("random.sml"),
];
