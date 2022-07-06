signature UNIX_SOCK_UTIL = sig
  include SOCK_UTIL
  val connectUnixStrm : string -> UnixSock.unix stream_sock
end

structure UnixSockUtil : UNIX_SOCK_UTIL = struct end
