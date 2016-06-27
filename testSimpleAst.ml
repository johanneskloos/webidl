let () =
  let open Arg in
  let files = ref [] in
    Arg.parse [
    ] (fun arg -> files := arg :: !files)
      "testSimpleAst filenames";
    let asts =
      List.rev_map (fun filename -> Parse.read_ast_from_file filename) !files
    in let simple_ast = AstSimplify.cleanup (List.concat asts)
    in Format.printf "@[<v>%a@ @]"
         (Fmt.list ~sep:Fmt.cut SimpleAst.pp_definition)
         simple_ast

