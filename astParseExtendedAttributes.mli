type ctx = AstParseError.ctx
type 'a arg_handler
val xattr_plain : string ->
  (ctx -> 'a -> 'a) -> 
  string * 'a arg_handler
val xattr_equals : string ->
  (ctx -> 'a -> string -> 'a) ->
  string * 'a arg_handler
val xattr_maybe_arguments : string ->
  (ctx -> 'a -> SimpleAst.arguments option -> 'a) ->
  string * 'a arg_handler
val xattr_equals_maybe_arguments : string ->
  (ctx -> 'a -> string -> SimpleAst.arguments option -> 'a) ->
  string * 'a arg_handler
val xattr_equals_specific : string ->
  (string * ('a -> 'a)) list ->
  string * 'a arg_handler
val handle_non_failing_known : 'a -> ctx ->
  (string * 'a arg_handler) list ->
  SimpleAst.extended_attribute_list -> 'a * SimpleAst.extended_attribute_list
val handle_all_known : 'a -> ctx ->
  (string * 'a arg_handler) list ->
  SimpleAst.extended_attribute_list -> 'a * SimpleAst.extended_attribute_list
val partition_attributes : string list ->
  SimpleAst.extended_attribute_list -> SimpleAst.extended_attribute_list * SimpleAst.extended_attribute_list
val keep_good_attributes : string list ->
  SimpleAst.extended_attribute_list -> SimpleAst.extended_attribute_list
val drop_bad_attributes : ctx -> string list ->
  SimpleAst.extended_attribute_list -> SimpleAst.extended_attribute_list

