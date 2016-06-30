open Parse

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

