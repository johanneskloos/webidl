module A = CalculateBlocking.Blacklist
open TypeGraph

let pp_vertex (pp: Format.formatter) = let open Fmt in function
  | Vertex.Global -> string pp "global"
  | Vertex.Class qn -> string pp "class "; IdlData.pp_qualified_name pp qn
  | Vertex.Instance qn -> string pp "instance "; IdlData.pp_qualified_name pp qn

let pp_edge_type pp = let open Fmt in function
  | Edge.Attribute a -> string pp "attribute "; string pp a
  | Edge.Result r -> string pp "result from "; string pp r

let pp_edge_mark pp = let open Fmt in function
  | Edge.Good -> ()
  | Edge.Blacklisted -> string pp " (blacklisted)"
  | Edge.Nondeterministic -> string pp " (nondeterministic)"

let pp_edge pp (src, (lbl, mark), tgt) =
  Fmt.pf pp "@[<hov 4>%a@ --@[<h> %a%a@] ->@ %a@]"
    pp_vertex src pp_edge_type lbl pp_edge_mark mark pp_vertex tgt

let () =
  let open Fmt in
  let open Arg in Format.set_margin max_int;
  let files = ref []  in
    Arg.parse []
      (fun arg -> files := arg :: !files)
      "testGraph filenames";
    let defs = Webidl.flatten_from_files !files
    in let graph = TypeGraph.build_reachability_graph defs
    in let reachable = A.find_attribute_reachable graph
    and is_vertex_bad = A.is_vertex_bad graph
    and reaches_bad = A.find_reaches_bad graph
    and blocked = A.calculate_block graph
    and paths = A.calculate_blocking_paths graph
    in let pp_vertex_results pp v =
      Fmt.char pp (if reachable v then 'R' else ' ');
      Fmt.char pp (if is_vertex_bad v then 'B' else ' ');
      Fmt.char pp (if reaches_bad v then 'X' else ' ');
      Fmt.char pp ' ';
      pp_vertex pp v
    and pp_edge_results pp e =
      Fmt.char pp (if blocked e then 'B' else ' ');
      Fmt.char pp ' ';
      pp_edge pp e
    in Format.printf
         "@[<v>Vertex analysis@ ===============@ %a@ Edge analysis@ =============@ %a@ @ Blacklisted paths@ %a@]"
         (iter ~sep:cut TypeGraph.G.iter_vertex pp_vertex_results)
         graph
         (iter ~sep:cut TypeGraph.G.iter_edges_e pp_edge_results)
         graph
         (list ~sep:cut (pair ~sep:(const string " ") (Fmt.list ~sep:(const string ".") string) string))
         paths
