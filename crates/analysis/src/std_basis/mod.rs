use statics::Statics;

/// A standard basis.
#[derive(Debug, Clone)]
pub struct StdBasis {
  pub(crate) statics: Statics,
}

impl StdBasis {
  /// The minimal standard basis. Only includes fundamental top-level definitions like `int`,
  /// `real`, `ref`, `<`, etc.
  pub fn minimal() -> Self {
    Self {
      statics: Statics::default(),
    }
  }

  /// The full standard basis, as documented in the public SML basis library docs.
  pub fn full() -> Self {
    elapsed::log("get_full_std_basis", get_full_std_basis)
  }
}

fn get_full_std_basis() -> StdBasis {
  let mut statics = Statics::default();
  for &contents in ORDER {
    let lexed = lex::get(contents);
    if let Some(e) = lexed.errors.first() {
      panic!("lex error: {}", e.display());
    }
    let parsed = parse::get(&lexed.tokens);
    if let Some(e) = parsed.errors.first() {
      panic!("parse error: {}", e.display());
    }
    let mut lowered = lower::get(&parsed.root);
    if let Some(e) = lowered.errors.first() {
      panic!("lower error: {}", e.display());
    }
    ty_var_scope::get(&mut lowered.arenas, &lowered.top_decs);
    let mode = statics::Mode::StdBasis;
    let (_, es) = statics::get(&mut statics, mode, &lowered.arenas, &lowered.top_decs);
    if let Some(e) = es.first() {
      panic!("statics error: {}", e.display(statics.syms()));
    }
  }
  statics.condense();
  StdBasis { statics }
}

macro_rules! order {
  ( $( $x:literal , )* ) => {{
    &[
      $(
        include_str!($x),
      )*
    ]
  }};
}

const ORDER: &[&str] = order!(
  "general.sml",
  "option.sml",
  "list.sml",
  "list-pair.sml",
  "string-cvt.sml",
  "integer.sml",
  "bool.sml",
  "vector.sml",
  "vector-slice.sml",
  "array.sml",
  "array-slice.sml",
  "array2.sml",
  "command-line.sml",
  "ieee-float.sml",
  "io.sml",
  "math.sml",
  "real.sml",
  "word.sml",
  "string.sml",
  "char.sml",
  "substring.sml",
  "mono-vector.sml",
  "mono-vector-slice.sml",
  "mono-array.sml",
  "mono-array-slice.sml",
  "mono-array2.sml",
  "int-inf.sml",
  "byte.sml",
  "stream-io.sml",
  "time.sml",
  "date.sml",
  "timer.sml",
  "os-file-sys.sml",
  "os-io.sml",
  "os-path.sml",
  "os-process.sml",
  "os.sml",
  "prim-io.sml",
  "imperative-io.sml",
  "bin-io.sml",
  "bit-flags.sml",
  "pack-float.sml",
  "pack-word.sml",
  "net-host-db.sml",
  "socket.sml",
  "generic-sock.sml",
  "inet-sock.sml",
  "posix-error.sml",
  "posix-file-sys.sml",
  "posix-io.sml",
  "posix-proc-env.sml",
  "posix-process.sml",
  "posix-signal.sml",
  "posix-sys-db.sml",
  "posix-tty.sml",
  "posix.sml",
  "prot-db.sml",
  "serv-db.sml",
  "text-stream-io.sml",
  "text-io.sml",
  "text.sml",
  "unix-sock.sml",
  "unix.sml",
  "windows.sml",
  // non-standard
  "fn.sml",
  "random.sml",
);
