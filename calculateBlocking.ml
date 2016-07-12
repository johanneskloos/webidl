module type Criterion = sig
  val criterion: TypeGraph.Edge.edgemark -> bool
end
module Make(C: Criterion) = struct
  open TypeGraph
  module ReachableFromInitialWeight = struct
    type edge = G.edge
    type t = Inf | Finite
    let compare x y = match x, y with
      | Inf, Inf -> 0
      | Finite, Finite -> 0
      | Inf, Finite -> 1
      | Finite, Inf -> -1
    let add x y = match x, y with
      | Finite, Finite -> Finite
      | _, _ -> Inf
    let zero = Finite
    let weight (_, (edgetype, mark), _) =
      match edgetype with
        | Edge.Attribute _ -> if C.criterion mark then Inf else Finite
        | Edge.Result _ -> Inf
  end
  module ReachableFromInitial =
    Graph.Path.BellmanFord(G)(ReachableFromInitialWeight)
  let find_attribute_reachable g: Vertex.t -> bool =
    let h = ReachableFromInitial.all_shortest_paths g Vertex.Global in
      fun v ->
        try
          ReachableFromInitial.H.find h v = ReachableFromInitialWeight.Finite
        with Not_found -> false

  module ReachesBadAnalysis = struct
    type vertex = G.vertex
    type edge = G.edge
    type g = G.t
    type data = bool
    let direction = Graph.Fixpoint.Backward
    let equal: bool -> bool -> bool = (=)
    let join = (||)
    let analyze _ d = d
  end
  module ReachesBad = Graph.Fixpoint.Make(G)(ReachesBadAnalysis)

  let is_vertex_bad g v =
    G.fold_succ_e (fun (_, (_, mark), _) is_bad -> is_bad || C.criterion mark)
      g v false

  let find_reaches_bad g: Vertex.t -> bool =
    ReachesBad.analyze (is_vertex_bad g) g

  let is_block reachable_pred reaches_bad_pred
        ((src, (_, mark), tgt): G.edge) =
    reachable_pred src &&
    if reachable_pred tgt then
      C.criterion mark
    else
      reaches_bad_pred src

  let calculate_block g =
    is_block (find_attribute_reachable g) (find_reaches_bad g)

  module Paths = Graph.Path.Dijkstra(G)(ReachableFromInitialWeight)

  let to_name = let open TypeGraph.Edge in function
    | Attribute name -> name
    | Result name -> name

  let edge_to_name (_, (lbl, _), _) = to_name lbl

  let calculate_blocking_paths g =
    let block = calculate_block g in
      G.fold_edges_e
        (fun ((src, (lbl, _), _) as e) paths ->
           if block e then begin
             let (path, _) = Paths.shortest_path g TypeGraph.Vertex.Global src
             in (BatList.map edge_to_name path, to_name lbl) :: paths
           end else paths)
        g []
end
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
module Blacklist =
  Make(struct
         open TypeGraph.Edge
         let criterion = function Blacklisted -> true | _ -> false
       end)
module Nondeterministic =
  Make(struct
         open TypeGraph.Edge
         let criterion = function Nondeterministic -> true | _ -> false
       end)

