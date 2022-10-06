(*!
This modules provides hooks for actions (e.g., closing files) to be performed at various important
points in the ML process, such as at process exit.
!*)
signature CLEAN_UP = sig
  (*!
  AtExportML The event that occurs when SMLofNJ.exportML is called. AtExportFn The event that occurs
  when SMLofNJ.exportFn is called. AtExit The event that occurs when the ML process exits. AtInit
  The event that occurs when a heap image created by SMLofNJ.exportML begins executing. AtInitFn The
  event that occurs when a heap image created by SMLofNJ.exportFn begins executing.
  !*)
  datatype when = AtExportML | AtExportFn | AtExit | AtInit | AtInitFn
  (*!
  atAll A list containing all the when values.
  !*)
  val atAll : when list
  (*!
  addCleaner (name, when, f) Install a new cleaner f with name name. From now on, whenever an event
  w in the when list occurs, f(w) will be called. If there was previously a cleaner (when',g)
  associated with that name, then SOME(when',g) will be removed and returned, otherwise NONE.
  !*)
  val addCleaner : (string * when list * (when -> unit)) -> (when list * (when -> unit)) option
  (*!
  removeCleaner name Remove and return any cleaner associated with name, or return NONE.
  !*)
  val removeCleaner : string -> (when list * (when -> unit)) option
  (*!
  clean when Execute all cleaners executed with event when. This function is automatically called by
  the system at the appropriate times, but may also be called by the user.
  !*)
  val clean : when -> unit
end
