//! A language server for Standard ML.

mod comm;
mod headers;
mod io;
mod state;

fn main() {
  let (s_msg, r_msg) = crossbeam_channel::unbounded();
  let (s_res, r_res) = crossbeam_channel::unbounded();
  let read_stdin = std::thread::Builder::new()
    .name("read_stdin".to_owned())
    .spawn(move || io::read_stdin(s_msg))
    .unwrap();
  let write_stdout = std::thread::Builder::new()
    .name("write_stdout".to_owned())
    .spawn(move || io::write_stdout(r_res))
    .unwrap();
  let mut st = state::State::new();
  let exit_ok = loop {
    let msg = r_msg.recv().unwrap();
    match msg {
      comm::Message::Request(req) => {
        let res = st.handle_req(req);
        s_res.send(res).unwrap();
      }
      comm::Message::Notification(notif) => match st.handle_notif(notif) {
        None => {}
        Some(x) => break x,
      },
    }
  };
  drop(r_msg);
  drop(s_res);
  read_stdin.join().unwrap();
  write_stdout.join().unwrap();
  if !exit_ok {
    std::process::exit(1);
  }
}
