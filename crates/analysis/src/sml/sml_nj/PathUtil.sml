signature PATH_UTIL = sig
  val findFile : string list -> string -> string option
  val findFiles : string list -> string -> string list
  val existsFile : (string -> bool) -> string list -> string -> string option
  val allFiles : (string -> bool) -> string list -> string -> string list
  val findExe : string list -> string -> string option
end

structure PathUtil : PATH_UTIL = struct end
