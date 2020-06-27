//! A language server for Standard ML.

mod handle;
mod headers;
mod io;
mod serde;

use crossbeam_channel::unbounded;
use std::thread::Builder;

fn main() {
  let (s_req, r_req) = unbounded();
  let (s_res, r_res) = unbounded();
  Builder::new()
    .name("read_stdin".to_owned())
    .spawn(|| io::read_stdin(s_req))
    .unwrap();
  Builder::new()
    .name("write_stdout".to_owned())
    .spawn(|| io::write_stdout(r_res))
    .unwrap();
  loop {
    let req = r_req.recv().unwrap();
    let res = handle::get(req);
    s_res.send(res).unwrap();
  }
}
