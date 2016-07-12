module type S = sig
  open TypeGraph
  val find_attribute_reachable :
    G.t -> Vertex.t -> bool
  val is_vertex_bad : G.t -> G.vertex -> bool
  val find_reaches_bad : G.t -> Vertex.t -> bool
  val is_block : (Vertex.t -> bool) ->
    (Vertex.t -> bool) -> G.edge -> bool
  val calculate_block : G.t -> G.edge -> bool
  val calculate_blocking_paths: G.t -> (string list * string) list
end
module Blacklist : S
module Nondeterministic : S

