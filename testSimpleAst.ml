let () =
  let open Arg in
  let files = ref [] in
    Arg.parse [
    ] (fun arg -> files := arg :: !files)
      "testSimpleAst filenames";
    List.iter (fun filename ->
                 Format.printf "@[<v>%s:@ %a@ @]" filename
                   (Fmt.list ~sep:Fmt.cut SimpleAst.pp_definition)
                   (AstSimplify.cleanup (Parse.read_ast_from_file filename)))
      !files

