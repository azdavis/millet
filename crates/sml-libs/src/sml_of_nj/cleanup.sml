signature CLEAN_UP = sig
  datatype when = AtExportML | AtExportFn | AtExit | AtInit | AtInitFn
  val atAll : when list
  val addCleaner : (string * when list * (when -> unit)) -> (when list * (when -> unit)) option
  val removeCleaner : string -> (when list * (when -> unit)) option
  val clean : when -> unit
end
