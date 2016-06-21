open Common
type qualified_name = string list
val pp_qualified_name: qualified_name Fmt.t
val compare_qualified_name: qualified_name -> qualified_name -> int
val equal_qualified_name: qualified_name -> qualified_name -> bool
module QNameMap: BatMap.S with type key = qualified_name
type int_length = Short | Long | LongLong
val pp_int_length :
  Format.formatter -> int_length -> Ppx_deriving_runtime.unit
val show_int_length : int_length -> Ppx_deriving_runtime.string
type out_of_range_behavior = Modulo | Clamp | Exception
val pp_out_of_range_behavior :
  Format.formatter -> out_of_range_behavior -> Ppx_deriving_runtime.unit
val show_out_of_range_behavior :
  out_of_range_behavior -> Ppx_deriving_runtime.string
type int_type = {
  length : int_length;
  unsigned : bool;
  out_of_range : out_of_range_behavior;
}
val pp_int_type : Format.formatter -> int_type -> Ppx_deriving_runtime.unit
val show_int_type : int_type -> Ppx_deriving_runtime.string
type float_type = { double : bool; unrestricted : bool; }
val pp_float_type :
  Format.formatter -> float_type -> Ppx_deriving_runtime.unit
val show_float_type : float_type -> Ppx_deriving_runtime.string
type undefined_transform = Undefined | Null | EmptyString
val pp_undefined_transform :
  Format.formatter -> undefined_transform -> Ppx_deriving_runtime.unit
val show_undefined_transform :
  undefined_transform -> Ppx_deriving_runtime.string
type string_behavior = {
  null_as_empty_string : bool;
  undefined_as : undefined_transform;
}
val pp_string_behavior :
  Format.formatter -> string_behavior -> Ppx_deriving_runtime.unit
val show_string_behavior : string_behavior -> Ppx_deriving_runtime.string
type types =
    IntType of int_type
  | FloatType of float_type
  | NamedType of string list
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
  | SequenceType of types
val pp_types : Format.formatter -> types -> Ppx_deriving_runtime.unit
val show_types : types -> Ppx_deriving_runtime.string
type value = Common.value
val pp_value : Format.formatter -> value -> Ppx_deriving_runtime.unit
val show_value : value -> Ppx_deriving_runtime.string
type argument_kind = Single | Multiple | Optional | Default of value
val pp_argument_kind :
  Format.formatter -> argument_kind -> Ppx_deriving_runtime.unit
val show_argument_kind : argument_kind -> Ppx_deriving_runtime.string
type argument = {
  name : string;
  types : types;
  kind : argument_kind;
  user_attributes : user_attribute list;
}
and user_attribute =
    UAPlain of string
  | UAEquals of string * string
  | UAArguments of string * argument list
  | UAArgumentsEquals of string * string * argument list
val pp_argument : Format.formatter -> argument -> Ppx_deriving_runtime.unit
val show_argument : argument -> Ppx_deriving_runtime.string
val pp_user_attribute :
  Format.formatter -> user_attribute -> Ppx_deriving_runtime.unit
val show_user_attribute : user_attribute -> Ppx_deriving_runtime.string
type inheritance_mode = Toplevel | InheritsFrom of string list | ArrayClass
val pp_inheritance_mode :
  Format.formatter -> inheritance_mode -> Ppx_deriving_runtime.unit
val show_inheritance_mode : inheritance_mode -> Ppx_deriving_runtime.string
type special_handling = {
  this_lenient : bool;
  this_implicit : bool;
  named_properties_object : bool;
  override_builtins : bool;
}
val pp_special_handling :
  Format.formatter -> special_handling -> Ppx_deriving_runtime.unit
val show_special_handling : special_handling -> Ppx_deriving_runtime.string
type constant = {
  name : string;
  types : types;
  value : value;
  user_attributes : user_attribute list;
}
val pp_constant : Format.formatter -> constant -> Ppx_deriving_runtime.unit
val show_constant : constant -> Ppx_deriving_runtime.string
type access =
    ReadWrite
  | ReadOnly
  | PutForwards of string
  | Replacable
  | Unforgable
val pp_access : Format.formatter -> access -> Ppx_deriving_runtime.unit
val show_access : access -> Ppx_deriving_runtime.string
type attribute = {
  name : string;
  types : types;
  lenient_this : bool;
  inherited : bool;
  access : access;
  user_attributes : user_attribute list;
}
val pp_attribute : Format.formatter -> attribute -> Ppx_deriving_runtime.unit
val show_attribute : attribute -> Ppx_deriving_runtime.string
type operation = {
  name : string;
  return : types;
  args : argument list;
  user_attributes : user_attribute list;
}
val pp_operation : Format.formatter -> operation -> Ppx_deriving_runtime.unit
val show_operation : operation -> Ppx_deriving_runtime.string
type legacy_caller = {
  return : types;
  args : argument list;
  user_attributes : user_attribute list;
}
val pp_legacy_caller :
  Format.formatter -> legacy_caller -> Ppx_deriving_runtime.unit
val show_legacy_caller : legacy_caller -> Ppx_deriving_runtime.string
type attributed_type = {
  types : types;
  user_attributes : user_attribute list;
}
val pp_attributed_type :
  Format.formatter -> attributed_type -> Ppx_deriving_runtime.unit
val show_attributed_type : attributed_type -> Ppx_deriving_runtime.string
type property_set = {
  getter : attributed_type option;
  deleter : attributed_type option;
  setter : (attributed_type * attributed_type) option;
  creator : (attributed_type * attributed_type) option;
}
val pp_property_set :
  Format.formatter -> property_set -> Ppx_deriving_runtime.unit
val show_property_set : property_set -> Ppx_deriving_runtime.string
type stringifer_mode =
    NoStringifier
  | InternalStringifer of string_behavior * user_attribute list
  | AttributeStringifier of string * string_behavior * user_attribute list
val pp_stringifer_mode :
  Format.formatter -> stringifer_mode -> Ppx_deriving_runtime.unit
val show_stringifer_mode : stringifer_mode -> Ppx_deriving_runtime.string
type constructor = {
  name : qualified_name;
  args : argument list;
  user_attributes : user_attribute list;
}
val pp_constructor :
  Format.formatter -> constructor -> Ppx_deriving_runtime.unit
val show_constructor : constructor -> Ppx_deriving_runtime.string
type interface = {
  inheritance_mode : inheritance_mode;
  name : qualified_name;
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
  user_attributes : user_attribute list;
}
val pp_interface : Format.formatter -> interface -> Ppx_deriving_runtime.unit
val show_interface : interface -> Ppx_deriving_runtime.string
type dictionary_entry = {
  name : string;
  types : types;
  default_value : value option;
  user_attributes : user_attribute list;
}
val pp_dictionary_entry :
  Format.formatter -> dictionary_entry -> Ppx_deriving_runtime.unit
val show_dictionary_entry : dictionary_entry -> Ppx_deriving_runtime.string
type dictionary = {
  name : qualified_name;
  inherits_from : qualified_name option;
  members : dictionary_entry list;
  user_attributes : user_attribute list;
}
val pp_dictionary :
  Format.formatter -> dictionary -> Ppx_deriving_runtime.unit
val show_dictionary : dictionary -> Ppx_deriving_runtime.string
type exception_member = {
  name : string;
  types : types;
  user_attributes : user_attribute list;
}
val pp_exception_member :
  Format.formatter -> exception_member -> Ppx_deriving_runtime.unit
val show_exception_member : exception_member -> Ppx_deriving_runtime.string
type exception_ = {
  name : qualified_name;
  inherits_from : qualified_name option;
  consts : constant list;
  members : exception_member list;
  not_exposed : bool;
  user_attributes : user_attribute list;
}
val pp_exception_ :
  Format.formatter -> exception_ -> Ppx_deriving_runtime.unit
val show_exception_ : exception_ -> Ppx_deriving_runtime.string
type enumerate = {
  name : qualified_name;
  values : string list;
  user_attributes : user_attribute list;
}
val pp_enumerate : Format.formatter -> enumerate -> Ppx_deriving_runtime.unit
val show_enumerate : enumerate -> Ppx_deriving_runtime.string
type callback = {
  name : qualified_name;
  return : types;
  args : argument list;
  treat_non_callable_as_null : bool;
  user_attributes : user_attribute list;
}
val pp_callback : Format.formatter -> callback -> Ppx_deriving_runtime.unit
val show_callback : callback -> Ppx_deriving_runtime.string
type definitions = {
  dictionaries : dictionary QNameMap.t;
  enumerations : enumerate QNameMap.t;
  interfaces : interface QNameMap.t;
  exceptions : exception_ QNameMap.t;
  callbacks : callback QNameMap.t;
  callback_interfaces : interface QNameMap.t;
  implements : (qualified_name * qualified_name) list;
}
val pp_definitions :
  Format.formatter -> definitions -> Ppx_deriving_runtime.unit
val show_definitions : definitions -> Ppx_deriving_runtime.string
