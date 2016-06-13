module StringMap = BatMap.Make(String)

type int_length = Short | Long | LongLong [@@deriving show]
type out_of_range_behavior = Modulo | Clamp | Exception [@@deriving show]
type int_type = {
  length : int_length;
  unsigned : bool;
  out_of_range : out_of_range_behavior;
} [@@deriving show]
type float_type = { double : bool; unrestricted : bool; } [@@deriving show]
type undefined_transform = Undefined | Null | EmptyString [@@deriving show]
type string_behavior = {
  null_as_empty_string : bool;
  undefined_as : undefined_transform;
} [@@deriving show]
type types =
    IntType of int_type
  | FloatType of float_type
  | NamedType of string
  | AnyType
  | VoidType
  | DOMStringType of string_behavior
  | DateType
  | ObjectType
  | OctetType
  | ByteType
  | BooleanType
  | UnionType of types list
  | ArrayType of types
  | OptionType of bool * types
  | NullableType of types
  | SequenceType of types [@@deriving show]
type value = Ast.value =
  | StringValue of string
  | BoolValue of bool
  | FloatValue of float
  | IntValue of int
  | NullValue [@@deriving show]

type argument_kind = Single | Multiple | Optional | Default of value [@@deriving show]
type argument = { name : string; types : types; kind : argument_kind; user_attributes: user_attribute list } [@@deriving show]
and user_attribute =
  | UAPlain of string
  | UAEquals of string * string
  | UAArguments of string * argument list
  | UAArgumentsEquals of string * string * argument list [@@deriving show]

type inheritance_mode = Toplevel | InheritsFrom of string | ArrayClass [@@deriving show]
type special_handling = {
  this_lenient : bool;
  this_implicit : bool;
  named_properties_object : bool;
  override_builtins : bool;
} [@@deriving show]
type constant = { name : string; types : types; value : value; user_attributes: user_attribute list } [@@deriving show]
type access =
    ReadWrite
  | ReadOnly
  | PutForwards of string
  | Replacable
  | Unforgable [@@deriving show]
type attribute = {
  name : string;
  types : types;
  lenient_this : bool;
  inherited : bool;
  access : access;
  user_attributes: user_attribute list
} [@@deriving show]
type operation = { name : string; return : types; args : argument list; user_attributes: user_attribute list } [@@deriving show]
type legacy_caller = { return : types; args : argument list; user_attributes: user_attribute list } [@@deriving show]
type attributed_type = { types: types; user_attributes: user_attribute list } [@@deriving show]
type property_set = {
  getter : attributed_type option;
  deleter : attributed_type option;
  setter : (attributed_type * attributed_type) option;
  creator : (attributed_type * attributed_type) option;
} [@@deriving show]
type stringifer_mode =
    NoStringifier
  | InternalStringifer of string_behavior * user_attribute list
  | AttributeStringifier of string * string_behavior * user_attribute list [@@deriving show]
type constructor = { name : string; args : argument list; user_attributes: user_attribute list } [@@deriving show]
type interface = {
  inheritance_mode : inheritance_mode;
  name : string;
  consts : constant list;
  attributes : attribute list;
  operations : operation list;
  static_operations : operation list;
  constructors : constructor list;
  special : special_handling;
  named_properties : property_set;
  indexed_properties : property_set;
  legacy_callers : legacy_caller list;
  not_exposed : bool;
  stringifier : stringifer_mode;
  user_attributes: user_attribute list
} [@@deriving show]
type dictionary_entry = {
  name : string;
  types : types;
  default_value : value option;
  user_attributes: user_attribute list
} [@@deriving show]
type dictionary = {
  name : string;
  inherits_from : string option;
  members : dictionary_entry list;
  user_attributes: user_attribute list
} [@@deriving show]
type exception_member = { name : string; types : types; user_attributes: user_attribute list } [@@deriving show]
type exception_ = {
  name : string;
  inherits_from : string option;
  consts : constant list;
  members : exception_member list;
  not_exposed : bool;
  user_attributes: user_attribute list
} [@@deriving show]
type enumerate = { name : string; values : string list; user_attributes: user_attribute list } [@@deriving show]
type callback = {
  name : string;
  return : types;
  args : argument list;
  treat_non_callable_as_null : bool;
  user_attributes: user_attribute list
} [@@deriving show]
let pp_string_map pp_content =
  let kvsep = Fmt.const Fmt.string ": "
  in let kvfmt = Fmt.pair ~sep:kvsep Fmt.string pp_content
  in let boxedkvfmt = Fmt.box ~indent:2 kvfmt
  in let fmtmap = Fmt.iter_bindings ~sep:Fmt.cut StringMap.iter boxedkvfmt
  in Fmt.vbox ~indent:2 fmtmap
type definitions = {
  dictionaries : dictionary StringMap.t [@printer pp_string_map pp_dictionary];
  enumerations : enumerate StringMap.t [@printer pp_string_map pp_enumerate];
  interfaces : interface StringMap.t [@printer pp_string_map pp_interface];
  exceptions : exception_ StringMap.t [@printer pp_string_map pp_exception_];
  callbacks : callback StringMap.t [@printer pp_string_map pp_callback];
  callback_interfaces : interface StringMap.t [@printer pp_string_map pp_interface];
  implements : (string * string) list;
} [@@deriving show]

