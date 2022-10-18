signature MLTON_EXN = sig
  (*!
   * addExnMessager f adds f as a pretty-printer to be used by General.exnMessage for converting
   * exceptions to strings. Messagers are tried in order from most recently added to least recently
   * added.
   *)
  val addExnMessager : (exn -> string option) -> unit
  (*!
   * history e returns call stack at the point that e was first raised. Each element of the list is
   * a file position. The elements are in reverse chronological order, i.e. the function called last
   * is at the front of the list. Will return [] unless the program is compiled with -const
   * 'Exn.keepHistory true'.
   *)
  val history : exn -> string list
  (*!
   * defaultTopLevelHandler e behaves as the default top level handler; that is, print out the
   * unhandled exception message for e and exit.
   *)
  val defaultTopLevelHandler : exn -> 'a
  (*!
   * getTopLevelHandler () gets the top level handler.
   *)
  val getTopLevelHandler : unit -> (exn -> unit)
  (*!
   * setTopLevelHandler f set the top level handler to the function f. The function f should not
   * raise an exception or return normally.
   *)
  val setTopLevelHandler : (exn -> unit) -> unit
  (*!
   * topLevelHandler e behaves as if the top level handler received the exception e.
   *)
  val topLevelHandler : exn -> 'a
end
