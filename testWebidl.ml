let () =
  let open Arg in
  let files = ref [] in
  let flatten = ref false in
    Arg.parse [
      ("-f", Set flatten, "Flatten IDL file")
    ] (fun arg -> files := arg :: !files)
      "testWebidl [-f] filenames";
    let defs =
      if !flatten then
        Webidl.flatten_from_files !files
      else
        Webidl.parse_from_files !files
    in
      Format.printf "@[<v>@[<h>%a:@]@ %a@ @]"
        (Fmt.list ~sep:(Fmt.const Fmt.string ", ") Fmt.string) !files
        IdlData.pp_definitions defs

