signature SML_OF_NJ = sig
  structure Cont : CONT
  structure IntervalTimer : INTERVAL_TIMER
  structure Internals : INTERNALS
  structure SysInfo : SYS_INFO
  structure Susp : SUSP
  structure Weak : WEAK
  val exportML : string -> bool
  val exportFn : (string * ((string * string list) -> OS.Process.status)) -> unit
  val getCmdName : unit -> string
  val getArgs : unit -> string list
  val getAllArgs : unit -> string list
  datatype 'a frag = QUOTE of string | ANTIQUOTE of 'a
  val exnHistory : exn -> string list
end

structure SMLofNJ : SML_OF_NJ = struct end
