let read_ast_from_lexbuf = Idlparser.main Idllexer.read
let read_ast_from_channel channel =
  try 
    let ast = read_ast_from_lexbuf (Lexing.from_channel channel) in
      close_in channel; ast
  with e -> close_in channel; raise e

let read_ast_from_file file = read_ast_from_channel (open_in file)

let parse_ast ast = ast |> AstSimplify.cleanup |> AstParse.translate_definitions
let parse_from_lexbuf x = x |> read_ast_from_lexbuf |> parse_ast
let parse_from_channel x = x |> read_ast_from_channel |> parse_ast
let parse_from_file x = x |> read_ast_from_file |> parse_ast

let flatten_ast ast = ast |> parse_ast |> Flatten.flatten
let flatten_from_lexbuf x = x |> parse_from_lexbuf |> Flatten.flatten
let flatten_from_channel x = x |> parse_from_channel |> Flatten.flatten
let flatten_from_file x = x |> parse_from_file |> Flatten.flatten

