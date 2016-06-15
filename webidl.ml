let run_parser lexbuf =
  let module I = Idlparser.MenhirInterpreter in
    (* Stolen from CompCert *)
  let state checkpoint =
    match Lazy.force (I.stack checkpoint) with
      | MenhirLib.General.Nil -> 0
      | MenhirLib.General.Cons(I.Element (s, _, _, _), _) -> I.number s
  and last_token = ref Idlparser.EOF
  in let lexer lexbuf =
    let token = Idllexer.read lexbuf in
      last_token := token; token
  in
    I.loop_handle (fun result -> result)
      (function I.Rejected -> failwith "Parser rejected input"
         | I.HandlingError e ->
             let s = state e in
               Format.eprintf "%a: %s. Last token: %a@."
                 Misc.pp_position lexbuf.Lexing.lex_start_p
                 (try IdlparserOutput.message s with Not_found -> "no errror message found")
                 TokenPrinter.pp_token !last_token
                 ;
               raise Idlparser.Error
         | _ -> failwith "Unexpected state in failure handler!"
      )
      (I.lexer_lexbuf_to_supplier lexer lexbuf)
      (Idlparser.Incremental.main lexbuf.Lexing.lex_curr_p)

let read_ast_from_lexbuf ?fname lexbuf =
  begin match fname with
    | Some fname ->
        let open Lexing in
          lexbuf.lex_start_p <- { lexbuf.lex_start_p with pos_fname = fname };
          lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = fname }
    | None -> ()
  end;
  run_parser lexbuf

let read_ast_from_channel ?fname channel =
  try 
    let ast = read_ast_from_lexbuf ?fname (Lexing.from_channel channel) in
      close_in channel; ast
  with e -> close_in channel; raise e

let read_ast_from_file file = read_ast_from_channel ~fname:file (open_in file)

let parse_ast ast = ast |> AstSimplify.cleanup |> AstParse.translate_definitions
let parse_from_lexbuf x = x |> read_ast_from_lexbuf |> parse_ast
let parse_from_channel x = x |> read_ast_from_channel |> parse_ast
let parse_from_file x = x |> read_ast_from_file |> parse_ast
let parse_from_files x = x |> List.map read_ast_from_file |> List.flatten |> parse_ast

let flatten_ast ast = ast |> parse_ast |> Flatten.flatten
let flatten_from_lexbuf x = x |> parse_from_lexbuf |> Flatten.flatten
let flatten_from_channel x = x |> parse_from_channel |> Flatten.flatten
let flatten_from_file x = x |> parse_from_file |> Flatten.flatten
let flatten_from_files x = x |> parse_from_files |> Flatten.flatten

