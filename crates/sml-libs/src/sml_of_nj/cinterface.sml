signature CINTERFACE = sig
  exception CFunNotFound of string
  val c_function : string -> string -> 'a -> 'b
  type c_function
  val bindCFun : (string * string) -> c_function
  type system_const = (int * string)
  exception SysConstNotFound of string
  val findSysConst : (string * system_const list) -> system_const option
  val bindSysConst : (string * system_const list) -> system_const
end
