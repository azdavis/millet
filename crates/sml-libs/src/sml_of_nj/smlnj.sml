signature SML_OF_NJ = sig
  (*!
  First-class continuations and their operators.
  !*)
  structure Cont : CONT
  (*!
  Delivers alarm signal at specified intervals.
  !*)
  structure IntervalTimer : INTERVAL_TIMER
  (*!
  Ways to tweak the runtime system and read-eval-print loop.
  !*)
  structure Internals : INTERNALS
  (*!
  What hardware and operating system are we running on?
  !*)
  structure SysInfo : SYS_INFO
  (*!
  Suspensions for lazy evaluation.
  !*)
  structure Susp : SUSP
  (*!
  Weak pointers that don't keep objects alive.
  !*)
  structure Weak : WEAK
  (*!
  exportML filename Dump the current state of the ML process into a heap image called
  filename.arch-opsys (where arch is the machine architecture, such as "sparc" or "x86", and opsys
  is the operating system, such as "solaris" or "win32"; and return false. When the heap image is
  loaded into an SML runtime system, (with sml @SMLload=filename.arch-opsys, computation will resume
  as if exportML had returned true.
  !*)
  val exportML : string -> bool
  (*!
  exportFn (filename, f) Dump the function f into a heap image called filename.arch-opsys (where
  arch is the machine architecture, such as "sparc" or "x86", and opsys is the operating system,
  such as "solaris" or "win32"; and then exit. When the heap image is loaded into an SML runtime
  system, (with sml @SMLload=filename.arch-opsys arg1 arg2 ..., computation will start with a call
  to f(arg0,[arg1,arg2,...]) where arg0 is the name of the executable file (in this example, the
  pathname expansion of sml). When f returns, the ML process terminates, with the process
  termination condition determined by the value returned by f.
  !*)
  val exportFn : (string * ((string * string list) -> OS.Process.status)) -> unit
  (*!
  getCmdName () Get the command name by which ML was invoked (in Unix, the "zeroth" command-line
  argument).
  !*)
  val getCmdName : unit -> string
  (*!
  getArgs () Get the command-line arguments from when ML was invoked, not including the "zeroth"
  argument (in Unix) that is the command name, and not including any arguments starting with @SML
  (which are directives to the ML runtime system).
  !*)
  val getArgs : unit -> string list
  (*!
  getAllArgs () Get all the arguments to the ML process, including the zeroth argument and also
  arguments starting with @SML (which are directives to the ML runtime system).
  !*)
  val getAllArgs : unit -> string list
  (*!
  A datatype used by the quote/antiquote mechanism for object-language parsing; see also
  Compiler.Control.quotation.
  !*)
  datatype 'a frag = QUOTE of string | ANTIQUOTE of 'a
  (*!
  exnHistory exn Return the exception history of exn, showing where it was first raised and through
  which exception-handlers it has been passed (or reraised). Each string in the list describes the
  source-file and line number of the raising (or reraising) code; the most recent handler (or
  reraise) is first, the earliest raising point is last. Code compiled with
  Compiler.Control.trackExn set to false does not generate exception histories.
  !*)
  val exnHistory : exn -> string list
end

structure SMLofNJ : SML_OF_NJ = struct end
