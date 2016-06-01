val parse_from_lexbuf : Lexing.lexbuf -> IdlData.definitions
val parse_from_channel : in_channel -> IdlData.definitions
val parse_from_file : string -> IdlData.definitions
val flatten_from_lexbuf : Lexing.lexbuf -> IdlData.definitions
val flatten_from_channel : in_channel -> IdlData.definitions
val flatten_from_file : string -> IdlData.definitions
