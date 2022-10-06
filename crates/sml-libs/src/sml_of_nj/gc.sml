signature GC = sig
  (*!
  doGC g Garbage-collect the heap to generation g. doGC(0) does a most-minor-generation collection,
  doGC(100000) does a most-major-generation collection, making sure to scavenge all garbage. In a
  typical installation, the highest meaningful argument value is approximately 6.
  !*)
  val doGC : int -> unit
  (*!
  messages b turns garbage collection messages on or off, depending on the value of the boolean b.
  For interactive systems (i.e., those created by exportML), messages are on by default, while
  messages are off by default for images created by exportFn. Note that this feature is only
  available in version 110.0.1, and later.
  !*)
  val messages : bool -> unit
end
