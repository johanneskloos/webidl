let pp_node pp = function
  | Accessible.Vertex.Global -> Fmt.string pp "(globals)"
  | Accessible.Vertex.Class str ->
      Format.fprintf pp "Prototype of %a" IdlData.pp_qualified_name str
  | Accessible.Vertex.Instance str ->
      Format.fprintf pp "Instances of %a" IdlData.pp_qualified_name str
let pp_mode pp = function
  | (Accessible.MarkingAnalysis.Neither, false) ->
      Fmt.string pp "harmless unreachable"
  | (Accessible.MarkingAnalysis.Nondeterministic, false) ->
      Fmt.string pp "nondeterministic unreachable"
  | (Accessible.MarkingAnalysis.Blacklisted, false) ->
      Fmt.string pp "blacklisted unreachable"
  | (Accessible.MarkingAnalysis.Neither, true) ->
      Fmt.string pp "harmless reachable"
  | (Accessible.MarkingAnalysis.Nondeterministic, true) ->
      Fmt.string pp "cut-off object because of nondeterminism"
  | (Accessible.MarkingAnalysis.Blacklisted, true) ->
      Fmt.string pp "cut-off object because of blacklisting"

let pp_marking =
  Fmt.pair ~sep:(Fmt.const Fmt.string " → ") pp_node pp_mode
let () =
  let open Arg in
  let files = ref [] in
    Arg.parse [] (fun arg -> files := arg :: !files)
      "testMark filenames";
    let defs = Webidl.flatten_from_files !files
    in let marked = Accessible.mark defs
    in Format.printf "@[v<2>Marking for @[<h>%a@]:@ %a@ @]"
         (Fmt.list ~sep:(Fmt.const Fmt.string ", ") Fmt.string) !files
         (Fmt.list ~sep:Fmt.cut (Fmt.hbox pp_marking)) marked
