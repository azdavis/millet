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
  "imperative-io-fn.sml",
  "prim-io-fn.sml",
  "stream-io-fn.sml",
];
