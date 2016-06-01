module StringMap: BatMap.S with type key = string

type int_length = Short | Long | LongLong
type out_of_range_behavior = Modulo | Clamp | Exception
type int_type = {
  length : int_length;
  unsigned : bool;
  out_of_range : out_of_range_behavior;
}
type float_type = { double : bool; unrestricted : bool; }
type undefined_transform = Undefined | Null | EmptyString
type string_behavior = {
  null_as_empty_string : bool;
  undefined_as : undefined_transform;
}
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
  | SequenceType of types
type value = Ast.value
type inheritance_mode = Toplevel | InheritsFrom of string | ArrayClass
type special_handling = {
  this_lenient : bool;
  this_implicit : bool;
  named_properties_object : bool;
  override_builtins : bool;
}
type constant = { name : string; types : types; value : value; }
type access =
    ReadWrite
  | ReadOnly
  | PutForwards of string
  | Replacable
  | Unforgable
type attribute = {
  name : string;
  types : types;
  lenient_this : bool;
  inherited : bool;
  access : access;
}
type argument_kind = Single | Multiple | Optional | Default of value
type argument = { name : string; types : types; kind : argument_kind; }
type operation = { name : string; return : types; args : argument list; }
type legacy_caller = { return : types; args : argument list; }
type property_set = {
  getter : types option;
  deleter : types option;
  setter : (types * types) option;
  creator : (types * types) option;
}
type stringifer_mode =
    NoStringifier
  | InternalStringifer of string_behavior
  | AttributeStringifier of string * string_behavior
type constructor = { name : string; args : argument list; }
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
}
type dictionary_entry = {
  name : string;
  types : types;
  default_value : value option;
}
type dictionary = {
  name : string;
  inherits_from : string option;
  members : dictionary_entry list;
}
type exception_member = { name : string; types : types; }
type exception_ = {
  name : string;
  inherits_from : string option;
  consts : constant list;
  members : exception_member list;
  not_exposed : bool;
}
type enumerate = { name : string; values : string list; }
type callback = {
  name : string;
  return : types;
  args : argument list;
  treat_non_callable_as_null : bool;
}
type definitions = {
  dictionaries : dictionary StringMap.t;
  enumerations : enumerate StringMap.t;
  interfaces : interface StringMap.t;
  exceptions : exception_ StringMap.t;
  callbacks : callback StringMap.t;
  callback_interfaces : interface StringMap.t;
  implements : (string * string) list;
}
