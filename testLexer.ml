open TokenPrinter

let () =
  let open Arg in
  let open Lexing in
  let files = ref [] in
    parse [
    ] (fun file -> files := file :: !files)
      "parseLexer files";
    List.iter (fun filename ->
                 let file = open_in filename
                 in try
                   let buf = Lexing.from_channel file in
                     buf.lex_start_p <- { buf.lex_start_p with pos_fname = filename };
                     buf.lex_curr_p <- { buf.lex_curr_p with pos_fname = filename };
                     Format.printf "@[<hov 2>Lexing %s:@ " filename;
                     let rec dump_tokens buf =
                       let token = Idllexer.read buf in
                         Format.printf "%a: %a@ " Misc.pp_position buf.lex_curr_p pp_token token;
                         if token <> EOF then dump_tokens buf
                     in dump_tokens buf;
                        Format.printf "@]";
                        close_in file
                 with e -> (close_in file; raise e))
      !files
