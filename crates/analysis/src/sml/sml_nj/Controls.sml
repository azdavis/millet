signature CONTROLS = sig
  type priority = int list
  type 'a control
  type 'a value_cvt = { tyName : string, fromString : string -> 'a option, toString : 'a -> string }
  val control : { name : string, pri : priority, obscurity : int, help : string, ctl : 'a ref } -> 'a control
  val genControl : { name : string, pri : priority, obscurity : int, help : string, default : 'a } -> 'a control
  exception ValueSyntax of {tyName : string, ctlName : string, value : string}
  val stringControl : 'a value_cvt -> 'a control -> string control
  val name : 'a control -> string
  val get : 'a control -> 'a
  val set : 'a control * 'a -> unit
  val set' : 'a control * 'a -> unit -> unit
  val help : 'a control -> string
  val info : 'a control -> {priority : priority, obscurity : int, help : string}
  val mkOptionFlag : { ctl : bool control, short : string, long : string option } -> unit GetOpt.opt_descr
  val mkOptionReqArg : { ctl : string control, arg : string, short : string, long : string option } -> unit GetOpt.opt_descr
  val mkOption : { ctl : string control, arg : string, default : string, short : string, long : string option } -> unit GetOpt.opt_descr
  val save'restore : 'a control -> unit -> unit
  val compare : ('a control * 'a control) -> order
end
structure Controls : CONTROLS = struct end
