signature SYS_INFO = sig
  exception UNKNOWN
  datatype os_kind = UNIX | WIN32 | MACOS | OS2 | BEOS
  (*!
  getOSKind () Tell what operating system ML is running under.
  !*)
  val getOSKind : unit -> os_kind
  (*!
  getOSName () Return the name of the operating system (e.g. "Solaris", "OSF/1").
  !*)
  val getOSName : unit -> string
  (*!
  getOSVersion () Return the operating-system version (e.g., "<unknown>").
  !*)
  val getOSVersion : unit -> string
  (*!
  getHostArch () Return the name of the host architecture (e.g., "ALPHA32").
  !*)
  val getHostArch : unit -> string
  (*!
  getTargetArch () Get the name of the architecture for which the compiler is generating code
  (differs from getHostArch() only when cross-compiling).
  !*)
  val getTargetArch : unit -> string
  (*!
  hasSoftwarePolling () Tell whether software polling is installed in the runtime system.
  !*)
  val hasSoftwarePolling : unit -> bool
  (*!
  hasMultiprocessing () Tell whether multiprocessing support is installed in the runtime system.
  !*)
  val hasMultiprocessing : unit -> bool
end

structure SysInfo : SYS_INFO = struct end
