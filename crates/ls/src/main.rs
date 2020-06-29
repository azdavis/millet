//! A language server for Standard ML.

mod comm;
mod headers;
mod io;
mod state;

fn main() {
  let (s_inc, r_inc) = crossbeam_channel::unbounded();
  let (s_out, r_out) = crossbeam_channel::unbounded();
  let read_stdin = std::thread::Builder::new()
    .name("read_stdin".to_owned())
    .spawn(move || io::read_stdin(s_inc))
    .unwrap();
  let write_stdout = std::thread::Builder::new()
    .name("write_stdout".to_owned())
    .spawn(move || io::write_stdout(r_out))
    .unwrap();
  let mut st = state::State::new();
  let exit_ok = loop {
    match r_inc.recv().unwrap() {
      comm::Incoming::Request(req) => {
        let res = st.handle_request(req);
        s_out.send(comm::Outgoing::Response(res)).unwrap();
      }
      comm::Incoming::Notification(notif) => match st.handle_notification(notif) {
        None => {}
        Some(action) => match action {
          state::Action::Exit(x) => break x,
          state::Action::Respond(x) => s_out.send(x).unwrap(),
        },
      },
    }
  };
  drop(r_inc);
  drop(s_out);
  read_stdin.join().unwrap();
  write_stdout.join().unwrap();
  if !exit_ok {
    std::process::exit(1);
  }
}
