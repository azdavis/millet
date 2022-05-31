//! Threads for handling I/O.

use crate::comm::{Incoming, Outgoing};
use crate::headers::content_length;
use crossbeam_channel::{Receiver, Sender};
use std::io::BufRead as _;
use std::io::Read as _;
use std::io::Write as _;

pub(crate) fn read_stdin(s: Sender<Incoming>) {
  let stdin = std::io::stdin();
  let mut stdin = stdin.lock();
  let mut buf = Vec::new();
  let mut content_len: Option<usize> = None;
  loop {
    buf.clear();
    assert_ne!(stdin.read_until(b'\n', &mut buf).unwrap(), 0);
    if let Some(n) = content_length(&buf) {
      content_len = Some(n);
      continue;
    }
    if buf != b"\r\n" {
      continue;
    }
    let n = match content_len.take() {
      None => continue,
      Some(x) => x,
    };
    buf = vec![0; n];
    stdin.read_exact(&mut buf).unwrap();
    let msg = match Incoming::try_parse(&buf) {
      None => continue,
      Some(x) => x,
    };
    if s.send(msg).is_err() {
      break;
    }
  }
}

pub(crate) fn write_stdout(r: Receiver<Outgoing>) {
  let stdout = std::io::stdout();
  let mut stdout = stdout.lock();
  for res in r {
    let buf = res.into_vec().unwrap();
    write!(stdout, "Content-Length: {}\r\n\r\n", buf.len()).unwrap();
    stdout.write_all(&buf).unwrap();
    stdout.flush().unwrap();
  }
}
