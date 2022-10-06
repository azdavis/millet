signature CINTERFACE = sig
  exception CFunNotFound of string
  (*!
  The abstract type for pointer-to-native-code-function. Sometimes it is useful to pass values of
  this type to other native-code functions.
  !*)
  val c_function : string -> string -> 'a -> 'b
  (*!
  The abstract type for pointer-to-native-code-function. Sometimes it is useful to pass values of
  this type to other native-code functions.
  !*)
  type c_function
  (*!
  bindCFun (modName, funName) Look up "modName.funName" in the runtime system's dictionary of
  native-code functions, and return pointer-to-native-code-function. Most users will want to use
  c_function instead, which returns an ML-callable function.
  !*)
  val bindCFun : (string * string) -> c_function
  (*!
  A type typically used for making lists of (sys-error-num, sys-error-message).
  !*)
  type system_const = (int * string)
  (*!
  Exception raised by bindSysConst.
  !*)
  exception SysConstNotFound of string
  (*!
  findSysConst (s, l) If the pair (i,s) is present in l, return SOME(i,s); else return NONE.
  !*)
  val findSysConst : (string * system_const list) -> system_const option
  (*!
  bindSysConst (s, l) If the pair (i,s) is present in l, return (i,s); else raise SysConstNotFound.
  !*)
  val bindSysConst : (string * system_const list) -> system_const
end
