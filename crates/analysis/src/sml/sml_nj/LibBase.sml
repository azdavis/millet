signature LIB_BASE = sig
  exception Unimplemented of string
  exception Impossible of string
  exception NotFound
  val failure : {module : string, func : string, msg : string} -> 'a
end

structure LibBase : LIB_BASE = struct end
