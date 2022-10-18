signature MLTON_GC = sig
  val collect : unit -> unit
  val pack : unit -> unit
  val setMessages : bool -> unit
  val setSummary : bool -> unit
  val unpack : unit -> unit
  structure Statistics : sig
    val bytesAllocated : unit -> IntInf.int
    val lastBytesLive : unit -> IntInf.int
    val numCopyingGCs : unit -> IntInf.int
    val numMarkCompactGCs : unit -> IntInf.int
    val numMinorGCs : unit -> IntInf.int
    val maxBytesLive : unit -> IntInf.int
  end
end
