signature CONTROL_SET = sig
  type 'a control = 'a Controls.control
  type ('a, 'b) control_set
  val new : unit -> ('a, 'b) control_set
  val member : (('a, 'b) control_set * Atom.atom) -> bool
  val find : (('a, 'b) control_set * Atom.atom) -> {ctl : 'a control, info : 'b} option
  val insert : (('a, 'b) control_set * 'a control * 'b) -> unit
  val remove : (('a, 'b) control_set * Atom.atom) -> unit
  val infoOf : ('a, 'b) control_set -> 'a control -> 'b option
  val listControls : ('a, 'b) control_set -> {ctl : 'a control, info : 'b} list
  val listControls' : (('a, 'b) control_set * int) -> {ctl : 'a control, info : 'b} list
  val app : ({ctl : 'a control, info : 'b} -> unit) -> ('a, 'b) control_set -> unit
  val stringControls : 'a Controls.value_cvt -> ('a, 'b) control_set -> (string, 'b) control_set
end
structure ControlSet : CONTROL_SET = struct end
