signature GRAPH_SCC = sig
  structure Nd : ORD_KEY
  type node = Nd.ord_key
  datatype component = SIMPLE of node | RECURSIVE of node list
  (* TODO what's digraph?
  val topOrder' : digraph -> component list
  val topOrder : digraph -> component list
  *)
end

functor GraphSCCFn (Nd: ORD_KEY) :> GRAPH_SCC where Nd = Nd = struct end
