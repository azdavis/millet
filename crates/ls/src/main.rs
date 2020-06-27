//! A language server for Standard ML.

mod headers;
mod io;
mod serde;
mod state;

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
  let mut st = state::State::new();
  loop {
    let req = r_req.recv().unwrap();
    let res = st.handle(req);
    s_res.send(res).unwrap();
  }
}
