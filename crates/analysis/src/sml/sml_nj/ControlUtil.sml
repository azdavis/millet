signature CONTROL_UTIL = sig
  structure Cvt : sig
    val int : int Controls.value_cvt
    val bool : bool Controls.value_cvt
    val real : real Controls.value_cvt
    val stringList : string list Controls.value_cvt
    val string : string Controls.value_cvt
  end
  structure EnvName : sig
    val toUpper : string -> string -> string
  end
end

structure ControlUtil : CONTROL_UTIL = struct end
