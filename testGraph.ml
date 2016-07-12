module G = TypeGraph.G
module Vertex = TypeGraph.Vertex
module Edge = TypeGraph.Edge
module GDot = struct
  include G
  let graph_attributes _ = []
  let default_vertex_attributes _ = []
  let vertex_label = function
    | Vertex.Global -> "Global"
    | Vertex.Instance qn -> "Instance " ^ Fmt.to_to_string IdlData.pp_qualified_name qn
    | Vertex.Class qn -> "Class " ^ Fmt.to_to_string IdlData.pp_qualified_name qn
  let vertex_name = function
    | Vertex.Global -> "G"
    | Vertex.Instance qn -> "I" ^ (BatString.join "_" qn)
    | Vertex.Class qn -> "C" ^ (BatString.join "_" qn)
  let vertex_attributes v = [ `Label (vertex_label v) ]
  let get_subgraph _ = None
  let default_edge_attributes _ = []
  let edge_attributes (_, (et, em), _) =
    [ `Style (match et with
                | Edge.Attribute _ -> `Solid
                | Edge.Result _ -> `Dashed);
      `Color (match em with
                | Edge.Good -> 0x000000
                | Edge.Nondeterministic -> 0x0000FF
                | Edge.Blacklisted -> 0xFF0000);
      `Label (match et with
                | Edge.Attribute name -> name
                | Edge.Result name -> name)
    ]
end
module Dot = Graph.Graphviz.Dot(GDot)

let () =
  let open Arg in
  let files = ref [] and outfile = ref None and reduce_graph = ref false in
    Arg.parse [("-o", String (fun x -> outfile := Some x), "Output file"); ("-r", Set reduce_graph, "Reduce graph")]
      (fun arg -> files := arg :: !files)
      "testGraph [-o output] filenames";
    let defs = Webidl.flatten_from_files !files
    in let graph = TypeGraph.build_reachability_graph defs
    in match !outfile with
      | None -> Dot.output_graph stdout graph
      | Some fn ->
          let chan = open_out fn in
            Dot.output_graph chan graph;
            close_out chan
