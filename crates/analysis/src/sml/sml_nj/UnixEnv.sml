signature UNIX_ENV = sig
  val getFromEnv : (string * string list) -> string option
  val getValue : {name : string, default : string, env : string list} -> string
  val removeFromEnv : (string * string list) -> string list
  val addToEnv : (string * string list) -> string list
  val environ : unit -> string list
  val getEnv : string -> string option
end

structure UnixEnv : UNIX_ENV = struct end
