signature CONTROL_REGISTRY = sig
  type registry
  type control_info = { envName : string option }
  val new : {help : string} -> registry
  val register : registry -> { ctl : string Controls.control, envName : string option } -> unit
  val registerSet : registry -> { ctls : (string, 'a) ControlSet.control_set, mkEnvName : string -> string option } -> unit
  val nest : registry -> { prefix : string option, pri : Controls.priority, obscurity : int, reg : registry } -> unit
  val control : registry -> string list -> string Controls.control option
  val init : registry -> unit
  datatype registry_tree = RTree of {
    path : string list,
    help : string,
    ctls : { ctl : string Controls.control, info : control_info } list, subregs : registry_tree list
  }
  val controls : (registry * int option) -> registry_tree
end

structure ControlRegistry : CONTROL_REGISTRY = struct end
