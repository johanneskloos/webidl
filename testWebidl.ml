let () =
  let open Arg in
  let files = ref [] in
  let flatten = ref false in
    Arg.parse [
      ("-f", Set flatten, "Flatten IDL file")
    ] (fun arg -> files := arg :: !files)
      "testWebidl [-f] filenames";
    if !flatten then
      List.iter (fun filename ->
                   Format.printf "@[<v>%s:@ %a@ @ @]" 
                     filename
                     IdlData.pp_definitions (Webidl.flatten_from_file filename))
        !files
    else
      List.iter (fun filename ->
                   Format.printf "@[<v>%s:@ %a@ @ @]" 
                     filename
                     IdlData.pp_definitions (Webidl.parse_from_file filename))
        !files

