//! Threads for handling I/O.

use crate::headers::content_length;
use crate::serde::{Request, Response};
use crossbeam_channel::{Receiver, RecvError, SendError, Sender};
use std::io::BufRead as _;
use std::io::Read as _;
use std::io::Write as _;

pub fn read_stdin(s: Sender<Request>) {
  let stdin = std::io::stdin();
  let mut stdin = stdin.lock();
  let mut buf = Vec::new();
  let mut content_len: Option<usize> = None;
  loop {
    buf.clear();
    if stdin.read_until(b'\n', &mut buf).unwrap() == 0 {
      break;
    }
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
    let req = match Request::try_parse(&buf) {
      None => continue,
      Some(x) => x,
    };
    match s.send(req) {
      Ok(()) => {}
      Err(SendError(_)) => break,
    }
  }
}

#[allow(clippy::while_let_loop)]
pub fn write_stdout(r: Receiver<Response>) {
  let stdout = std::io::stdout();
  let mut stdout = stdout.lock();
  loop {
    let res = match r.recv() {
      Ok(x) => x,
      Err(RecvError) => break,
    };
    let buf = res.into_vec().unwrap();
    write!(stdout, "Content-Length: {}\r\n\r\n", buf.len()).unwrap();
    stdout.write_all(&buf).unwrap();
    stdout.flush().unwrap();
  }
}
