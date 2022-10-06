signature PROF_CONTROL = sig
  val getTimeArray : unit -> int array
  val profMode : bool ref
  val current : int ref
  val profileOn : unit -> unit
  val profileOff : unit -> unit
  val getTimingMode : unit -> bool
  val getQuantum : unit -> int
  datatype compunit = UNIT of {base : int, size : int, counts : int Array.array, names : string}
  val runTimeIndex : int
  val minorGCIndex : int
  val majorGCIndex : int
  val otherIndex : int
  val compileIndex : int
  val numPredefIndices : int
  val units : compunit list ref
  val reset : unit -> unit
  val spaceProfiling : bool ref
  val spaceProfRegister : ((Unsafe.Object.object * string) -> Unsafe.Object.object) ref
end
