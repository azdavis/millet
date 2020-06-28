//! A language server for Standard ML.

mod headers;
mod io;
mod serde;
mod state;

fn main() {
  let (s_req, r_req) = crossbeam_channel::unbounded();
  let (s_res, r_res) = crossbeam_channel::unbounded();
  let read_stdin = std::thread::Builder::new()
    .name("read_stdin".to_owned())
    .spawn(move || io::read_stdin(s_req))
    .unwrap();
  let write_stdout = std::thread::Builder::new()
    .name("write_stdout".to_owned())
    .spawn(move || io::write_stdout(r_res))
    .unwrap();
  let mut st = state::State::new();
  loop {
    let req = r_req.recv().unwrap();
    let res = match st.handle(req) {
      Some(x) => x,
      None => break,
    };
    s_res.send(res).unwrap();
  }
  drop(r_req);
  drop(s_res);
  read_stdin.join().unwrap();
  write_stdout.join().unwrap();
}
