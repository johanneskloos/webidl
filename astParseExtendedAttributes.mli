type ctx = AstParseError.ctx
type 'a arg_handler
val xattr_plain : string ->
  (ctx -> 'a -> 'a) -> 
  string * 'a arg_handler
val xattr_equals : string ->
  (ctx -> 'a -> string -> 'a) ->
  string * 'a arg_handler
val xattr_maybe_arguments : string ->
  (ctx -> 'a -> Ast.argument_list option -> 'a) ->
  string * 'a arg_handler
val xattr_equals_maybe_arguments : string ->
  (ctx -> 'a -> string -> Ast.argument_list option -> 'a) ->
  string * 'a arg_handler
val xattr_equals_specific : string ->
  (string * ('a -> 'a)) list ->
  string * 'a arg_handler
val handle_non_failing_known : 'a -> ctx ->
  (string * 'a arg_handler) list ->
  Ast.extended_attribute list -> 'a * Ast.extended_attribute list
val handle_all_known : 'a -> ctx ->
  (string * 'a arg_handler) list ->
  Ast.extended_attribute list -> 'a * Ast.extended_attribute list
val partition_attributes : string list ->
  Ast.extended_attribute list -> Ast.extended_attribute list * Ast.extended_attribute list
val keep_good_attributes : string list ->
  Ast.extended_attribute list -> Ast.extended_attribute list
val drop_bad_attributes : ctx -> string list ->
  Ast.extended_attribute list -> Ast.extended_attribute list

