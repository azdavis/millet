//! The SML files here were mostly generated via [sml-basis][] from the public SML basis
//! documentation [website][].
//!
//! The website demands that a copyright notice accompany distributions of the documentation. It is
//! reproduced below, though I am not sure if the files I've generated constitute a "copy" or
//! "distribution", since I (azdavis) transformed the HTML into SML via my own original code.
//!
//! > Copyright © 2004 AT&T and Lucent Technologies. All rights reserved.
//! >
//! > Permission is granted for internet users to make one paper copy for their own personal use.
//! > Further hardcopy reproduction is strictly prohibited. Permission to distribute the HTML
//! > document electronically on any medium other than the internet must be requested from the
//! > copyright holders by contacting the editors. Printed versions of the SML Basis Manual are
//! > available from Cambridge University Press. To order, please visit www.cup.org (North America)
//! > or www.cup.cam.ac.uk (outside North America).
//!
//! [sml-basis]: https://github.com/azdavis/sml-basis
//! [website]: https://smlfamily.github.io/Basis/

use crate::files;

/// The files.
pub const FILES: &[(&str, &str)] = files![
  "std_basis/general.sml",
  "std_basis/option.sml",
  "std_basis/list.sml",
  "std_basis/list-pair.sml",
  "std_basis/string-cvt.sml",
  "std_basis/integer.sml",
  "std_basis/bool.sml",
  "std_basis/vector.sml",
  "std_basis/vector-slice.sml",
  "std_basis/array.sml",
  "std_basis/array-slice.sml",
  "std_basis/array2.sml",
  "std_basis/command-line.sml",
  "std_basis/ieee-float.sml",
  "std_basis/io.sml",
  "std_basis/math.sml",
  "std_basis/real.sml",
  "std_basis/word.sml",
  "std_basis/string.sml",
  "std_basis/char.sml",
  "std_basis/substring.sml",
  "std_basis/mono-vector.sml",
  "std_basis/mono-vector-slice.sml",
  "std_basis/mono-array.sml",
  "std_basis/mono-array-slice.sml",
  "std_basis/mono-array2.sml",
  "std_basis/int-inf.sml",
  "std_basis/byte.sml",
  "std_basis/stream-io.sml",
  "std_basis/time.sml",
  "std_basis/date.sml",
  "std_basis/timer.sml",
  "std_basis/os-file-sys.sml",
  "std_basis/os-io.sml",
  "std_basis/os-path.sml",
  "std_basis/os-process.sml",
  "std_basis/os.sml",
  "std_basis/prim-io.sml",
  "std_basis/imperative-io.sml",
  "std_basis/bin-io.sml",
  "std_basis/bit-flags.sml",
  "std_basis/pack-float.sml",
  "std_basis/pack-word.sml",
  "std_basis/net-host-db.sml",
  "std_basis/socket.sml",
  "std_basis/generic-sock.sml",
  "std_basis/inet-sock.sml",
  "std_basis/posix-error.sml",
  "std_basis/posix-file-sys.sml",
  "std_basis/posix-io.sml",
  "std_basis/posix-proc-env.sml",
  "std_basis/posix-process.sml",
  "std_basis/posix-signal.sml",
  "std_basis/posix-sys-db.sml",
  "std_basis/posix-tty.sml",
  "std_basis/posix.sml",
  "std_basis/prot-db.sml",
  "std_basis/serv-db.sml",
  "std_basis/text-stream-io.sml",
  "std_basis/text-io.sml",
  "std_basis/text.sml",
  "std_basis/unix-sock.sml",
  "std_basis/unix.sml",
  "std_basis/windows.sml",
  "std_basis/imperative-io-fn.sml",
  "std_basis/prim-io-fn.sml",
  "std_basis/stream-io-fn.sml",
];
