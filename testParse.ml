let () =
  let open Arg in
  let files = ref [] in
    Arg.parse [
    ] (fun arg -> files := arg :: !files)
      "testSimpleAst filenames";
    let asts =
      List.rev_map (fun filename -> Parse.read_ast_from_file filename) !files
    in let ast = List.concat asts
    in Format.printf "@[<v>%a@ @]"
         (Fmt.list ~sep:Fmt.cut Ast.pp_definition)
         ast

