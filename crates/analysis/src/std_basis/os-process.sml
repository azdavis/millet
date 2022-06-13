signature OS_PROCESS = sig
  type status
  val success : status
  val failure : status
  val isSuccess : status -> bool
  val system : string -> status
  val atExit : (unit -> unit) -> unit
  val exit : status -> 'a
  val terminate : status -> 'a
  val getEnv : string -> string option
  val sleep : Time.time -> unit
end

structure Process : OS_PROCESS = struct end
