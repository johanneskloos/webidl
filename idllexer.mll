{
    open Lexing
    open Idlparser

    let next_line lexbuf = Lexing.new_line lexbuf
}

let decdigit=['0'-'9']
let hexdigit=['0'-'9' 'A' - 'F' 'a' - 'F']
let octdigit=['0'-'7']
let decint = '-'? ['1'-'9'] decdigit*
let hexint = '-'? '0' ['X' 'x'] hexdigit+
let octint = '0' octdigit*
let int = decint | hexint | octint
let float_no_exp = decdigit+ '.' decdigit* | '.' decdigit+
let float_exp_part = ['E' 'e'] ['+' '-']? decdigit+
let float = '-'? (float_no_exp float_exp_part? | decdigit+ float_exp_part)
let identifier = ['A'-'Z' 'a'-'z'] ['0'-'9' 'A'-'Z' 'a'-'z']*
let whitespace = ['\t''\r'' ']+

rule read = parse
  | '#' identifier { skip_to_eol lexbuf }
  | '\n' { next_line lexbuf; read lexbuf }
  | whitespace { read lexbuf }
  | '/' '*' { skip_comment lexbuf }
  | '/' '/' { skip_to_eol lexbuf }
  | "void" { Void }
  | "valuetype" { Valuetype }
  | "unsigned" { Unsigned }
  | "unrestricted" { Unrestricted }
  | "typedef" { Typedef }
  | "true" { True }
  | "stringifier" { Stringifier }
  | '"' { read_string (Buffer.create 17) lexbuf }
  | "static" { Static }
  | "short" { Short }
  | "setter" { Setter }
  | "setraises" { SetRaises }
  | "sequence" { Sequence }
  | ";" { Semicolon }
  | "readonly" { ReadOnly }
  | "raises" { Raises }
  | ")" { RPar }
  | "]" { RBracket }
  | "}" { RBrace }
  | "?" { Question }
  | "partial" { Partial }
  | "optional" { Optional }
  | "omittable" { Omittable }
  | "octet" { Octet }
  | "object" { Object }
  | "or" { OR }
  | "null" { Null }
  | "NaN" { NaN }
  | "module" { Module }
  | "-" { Minus }
  | "<" { Lt }
  | "long" { Long }
  | "legacycaller" { LegacyCaller }
  | "(" { LPar }
  | "[" { LBracket }
  | "{" { LBrace }
  | "interface" { Interface }
  | "inherits" { Inherits }
  | "inherit" { Inherit }
  | "Infinity" { Infinity }
  | "in" { In }
  | "inout" { InOut }
  | "implements" { Implements }
  | ">" { Gt }
  | "getter" { Getter }
  | "getraises" { GetRaises }
  | "float" { Float }
  | "false" { False }
  | "exception" { Exception }
  | "=" { Equals }
  | "enum" { Enum }
  | "..." { Ellipsis }
  | "::" { DoubleColon }
  | "double" { Double }
  | "dictionary" { Dictionary }
  | "deleter" { Deleter }
  | "Date" { Date }
  | "DOMString" { DOMString }
  | "creator" { Creator }
  | "const" { Const }
  | "," { Comma }
  | ":" { Colon }
  | "callback" { Callback }
  | "byte" { Byte }
  | "boolean" { Boolean }
  | "attribute" { Attribute }
  | "any" { Any }
  | float { FloatValue (float_of_string (Lexing.lexeme lexbuf)) } 
  | int { IntegerValue (int_of_string (Lexing.lexeme lexbuf)) }
  | identifier { Identifier (Lexing.lexeme lexbuf) }
  | eof { EOF }
and read_string buf = parse
  | '\n' { next_line lexbuf; Buffer.add_char buf '\n'; read_string buf lexbuf }
  | '"' { String (Buffer.contents buf) }
  | [^'"''\n']* { Buffer.add_string buf (Lexing.lexeme lexbuf); read_string buf lexbuf }
  | eof { failwith "Unterminated string" }
and skip_comment = parse
  | '\n' { next_line lexbuf; skip_comment lexbuf }
  | "*/" { read lexbuf }
  | '*' [^'/''\n'] { skip_comment lexbuf }
  | '*' '\n' { next_line lexbuf; skip_comment lexbuf }
  | [^'*''\n']* { skip_comment lexbuf }
  | eof { failwith "Unterminated comment" }
and skip_to_eol = parse
  | '\n' { next_line lexbuf; read lexbuf }
  | eof { EOF }
  | [^'\n']* { skip_to_eol lexbuf }
