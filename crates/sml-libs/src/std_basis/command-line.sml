(*!
 * The CommandLine structure provides access to the name and arguments used to invoke the currently
 * running program.
 *)
signature COMMAND_LINE = sig
  (*!
   * The name used to invoke the current program.
   *)
  val name : unit -> string
  (*!
   * The argument list used to invoke the current program. Implementation note: The arguments
   * returned may be only a subset of the arguments actually supplied by the user, since an
   * implementation's runtime system may consume some of them.
   *)
  val arguments : unit -> string list
end

structure CommandLine :> COMMAND_LINE = struct end
