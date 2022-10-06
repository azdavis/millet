structure ANSITerm : sig
  datatype color =
    Black
  | Red
  | Green
  | Yellow
  | Blue
  | Magenta
  | Cyan
  | White
  | Default
  datatype style =
    FG of color
  | BG of color
  | BF
  | DIM
  | NORMAL
  | UL
  | UL_OFF
  | BLINK
  | BLINK_OFF
  | REV
  | REV_OFF
  | INVIS
  | INVIS_OFF
  | RESET
  val toString : style list -> string
  val setStyle : TextIO.outstream * style list -> unit
end = struct end
