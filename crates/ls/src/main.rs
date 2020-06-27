//! A language server for Standard ML.

mod handle;
mod headers;
mod io;
mod serde;

fn main() {
  let (s_req, r_req) = crossbeam_channel::unbounded();
  let (s_res, r_res) = crossbeam_channel::unbounded();
  std::thread::Builder::new()
    .name("read_stdin".to_owned())
    .spawn(|| io::read_stdin(s_req))
    .unwrap();
  std::thread::Builder::new()
    .name("write_stdout".to_owned())
    .spawn(|| io::write_stdout(r_res))
    .unwrap();
  let mut st = handle::State::new();
  loop {
    let req = r_req.recv().unwrap();
    let res = handle::get(&mut st, req);
    s_res.send(res).unwrap();
  }
}
