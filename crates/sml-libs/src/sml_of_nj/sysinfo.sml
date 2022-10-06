signature SYS_INFO = sig
  exception UNKNOWN
  datatype os_kind = UNIX | WIN32 | MACOS | OS2 | BEOS
  val getOSKind : unit -> os_kind
  val getOSName : unit -> string
  val getOSVersion : unit -> string
  val getHostArch : unit -> string
  val getTargetArch : unit -> string
  val hasSoftwarePolling : unit -> bool
  val hasMultiprocessing : unit -> bool
end

structure SysInfo : SYS_INFO = struct end
