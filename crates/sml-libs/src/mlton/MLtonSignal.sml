signature MLTON_SIGNAL = sig
  type t = Posix.Signal.signal
  type signal = t
  structure Handler : sig
    type t
    val default : t
    val handler : (Thread.Runnable.t -> Thread.Runnable.t) -> t
    val ignore : t
    val isDefault : t -> bool
    val isIgnore : t -> bool
    val simple : (unit -> unit) -> t
    end
  structure Mask : sig
    type t
    val all : t
    val allBut : signal list -> t
    val block : t -> unit
    val getBlocked : unit -> t
    val isMember : t * signal -> bool
    val none : t
    val setBlocked : t -> unit
    val some : signal list -> t
    val unblock : t -> unit
  end
  val getHandler : t -> Handler.t
  val handled : unit -> Mask.t
  val prof : t
  val restart : bool ref
  val setHandler : t * Handler.t -> unit
  val suspend : Mask.t -> unit
  val vtalrm : t
end
