use once_cell::sync::Lazy;

const ORDER: &[&str] = &[
  include_str!("list-pair.sml"),
  include_str!("list.sml"),
  include_str!("option.sml"),
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
];

pub(crate) static SYMS_AND_BS: Lazy<(statics::Syms, statics::Bs)> = Lazy::new(|| {
  let mut st = statics::Statics::default();
  let mode = statics::Mode::Declaration;
  for &contents in ORDER {
    let lexed = lex::get(contents);
    if let Some(e) = lexed.errors.first() {
      panic!("std_basis error: {}", e.kind);
    }
    let parsed = parse::get(&lexed.tokens);
    if let Some(e) = parsed.errors.first() {
      panic!("std_basis error: {}", e.kind);
    }
    let mut lowered = lower::get(&parsed.root);
    if let Some(e) = lowered.errors.first() {
      panic!("std_basis error: {}", e.kind);
    }
    ty_var_scope::get(&mut lowered.arenas, &lowered.top_decs);
    statics::get(&mut st, mode, &lowered.arenas, &lowered.top_decs);
    if let Some(e) = st.errors.first() {
      panic!("std_basis error: {}", e.display(&st.syms));
    }
  }
  (st.syms, st.bs)
});
