signature MLTON_PLATFORM = sig
  structure Arch : sig
    datatype t = Alpha | AMD64 | ARM | ARM64 | HPPA | IA64 | m68k | MIPS | PowerPC | PowerPC64 | S390 | Sparc | X86
    val fromString : string -> t option
    val host : t
    val toString : t -> string
  end
  structure OS : sig
    datatype t = AIX | Cygwin | Darwin | FreeBSD | Hurd | HPUX | Linux | MinGW | NetBSD | OpenBSD | Solaris
    val fromString : string -> t option
    val host : t
    val toString : t -> string
  end
end
