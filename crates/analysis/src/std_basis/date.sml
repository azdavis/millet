signature DATE = sig
  datatype weekday = Mon | Tue | Wed | Thu | Fri | Sat | Sun
  datatype month = Jan | Feb | Mar | Apr | May | Jun | Jul | Aug | Sep | Oct | Nov | Dec
  type date
  exception Date
  val date : { year : int, month : month, day : int, hour : int, minute : int, second : int, offset : Time.time option } -> date
  val year : date -> int
  val month : date -> month
  val day : date -> int
  val hour : date -> int
  val minute : date -> int
  val second : date -> int
  val weekDay : date -> weekday
  val yearDay : date -> int
  val offset : date -> Time.time option
  val isDst : date -> bool option
  val localOffset : unit -> Time.time
  val fromTimeLocal : Time.time -> date
  val fromTimeUniv : Time.time -> date
  val toTime : date -> Time.time
  val compare : date * date -> order
  val fmt : string -> date -> string
  val toString : date -> string
  val scan : (char, 'a) StringCvt.reader -> (date, 'a) StringCvt.reader
  val fromString : string -> date option
end

structure Date :> DATE = struct end
