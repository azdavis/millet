(*!
 * The structure Posix.ProcEnv specifies functions, as described in Section 4 of the POSIX standard
 * 1003.1,1996[CITE], which provide primitive POSIX access to the process environment.
 *)
signature POSIX_PROC_ENV = sig
  (*!
   * A process ID, used as an identifier for an operating system process.
   *)
  eqtype pid
  (*!
   * User identifier.
   *)
  eqtype uid
  (*!
   * Group identifier.
   *)
  eqtype gid
  (*!
   * Open file descriptor.
   *)
  eqtype file_desc
  (*!
   * These functions convert between an abstract user ID and an underlying unique unsigned integer.
   * Note that wordToUid does not ensure that it returns a valid uid.
   *)
  val uidToWord : uid -> SysWord.word
  val wordToUid : SysWord.word -> uid
  (*!
   * These convert between an abstract group ID and an underlying unique unsigned integer. Note that
   * wordToGid does not ensure that it returns a valid gid.
   *)
  val gidToWord : gid -> SysWord.word
  val wordToGid : SysWord.word -> gid
  (*!
   * The process ID and the parent process ID, respectively, of the calling process.
   *)
  val getpid : unit -> pid
  val getppid : unit -> pid
  (*!
   * The real and effective user IDs, respectively, of the calling process.
   *)
  val getuid : unit -> uid
  val geteuid : unit -> uid
  (*!
   * The real and effective group IDs, respectively, of the calling process.
   *)
  val getgid : unit -> gid
  val getegid : unit -> gid
  (*!
   * setuid u sets the real user ID and effective user ID to u.
   *)
  val setuid : uid -> unit
  (*!
   * setgid g sets the real group ID and effective group ID to g.
   *)
  val setgid : gid -> unit
  (*!
   * The list of supplementary group IDs of the calling process.
   *)
  val getgroups : unit -> gid list
  (*!
   * The user name associated with the calling process, i.e., the login name associated with the
   * calling process.
   *)
  val getlogin : unit -> string
  (*!
   * The process group ID of the calling process.
   *)
  val getpgrp : unit -> pid
  (*!
   * This function creates a new session if the calling process is not a process group leader, and
   * returns the process group ID of the calling process.
   *)
  val setsid : unit -> pid
  (*!
   * See setpgid.
   *)
  val setpgid : {pid : pid option, pgid : pid option} -> unit
  (*!
   * A list of name-value pairs including, at least, the names: "sysname", "nodename", "release",
   * "version", and "machine". (A POSIX implementation may provide additional values beyond this
   * set.) The respective values are strings that describe the named system component.
   *)
  val uname : unit -> (string * string) list
  (*!
   * The elapsed wall time since the Epoch.
   *)
  val time : unit -> Time.time
  (*!
   * A record containing the wall time (elapsed), user time (utime), system time (stime), user CPU
   * time of terminated child processes (cutime), and system CPU time of terminated child processes
   * (cstime), for the calling process.
   *)
  val times : unit -> { elapsed : Time.time, utime : Time.time, stime : Time.time, cutime : Time.time, cstime : Time.time }
  (*!
   * getenv name searches the environment list for a string of the form name=value and returns
   * SOME(value) if name is present; it returns NONE if name is not present. This is equivalent to
   * OS.Process.getEnv.
   *)
  val getenv : string -> string option
  (*!
   * The environment of the calling process as a list of strings.
   *)
  val environ : unit -> string list
  (*!
   * A string that represents the pathname of the controlling terminal for the calling process.
   *)
  val ctermid : unit -> string
  (*!
   * ttyname fd produces a string that represents the pathname of the terminal associated with file
   * descriptor fd. It raises OS.SysErr if fd does not denote a valid terminal device.
   *)
  val ttyname : file_desc -> string
  (*!
   * isatty fd returns true if fd is a valid file descriptor associated with a terminal. Note that
   * isatty will return false if fd is a bad file descriptor.
   *)
  val isatty : file_desc -> bool
  (*!
   * sysconf s returns the integer value for the POSIX configurable system variable s. It raises
   * OS.SysErr if s does not denote a supported POSIX system variable. The properties required by
   * POSIX are described below. This list is a minimal set required for POSIX compliance, and an
   * implementation may extend it with additional properties.
   *
   * - "ARG_MAX" Maximum length of arguments, in bytes, for the functions Posix.Process.exec,
   *   Posix.Process.exece, and Posix.Process.execp. This also applies to environment data.
   * - "CHILD_MAX" Maximum number of concurrent processes associated with a real user ID.
   * - "CLK_TCK" Number of clock ticks per second.
   * - "NGROUPS_MAX" Maximum number of supplementary group IDs associated with a process, in addition
   *   to the effective group ID.
   * - "OPEN_MAX" Maximum number of files that one process can have open concurrently.
   * - "STREAM_MAX" Maximum number of streams that one process can have open concurrently.
   * - "TZNAME_MAX" Maximum number bytes allowed for a time zone name.
   * - "JOB_CONTROL" Non-zero if the implementation supports job control.
   * - "SAVED_IDS" Non-zero if each process has a saved set-user-ID and and saved set-group-ID.
   * - "VERSION" A version number. Consult Section 4.8 of POSIX standard 1003.1,1996 [CITE] for
   *   additional information. Note that a property in SML has the same name as the property in C, but
   *   without the prefix "_SC_".
   *)
  val sysconf : string -> SysWord.word
end

structure ProcEnv : POSIX_PROC_ENV = struct end
