module QualifiedName = struct
  type t = string list [@@deriving ord,eq]
end
type qualified_name = QualifiedName.t
let compare_qualified_name = QualifiedName.compare
let equal_qualified_name = QualifiedName.equal
let pp_qualified_name =
  let open Fmt in
    hbox (list ~sep:(const string ".") string)
module QNameMap = BatMap.Make(QualifiedName)

type out_of_range_behavior = Modulo | Clamp | Exception [@@deriving show]
type int_type = {
  length : Common.int_length;
  unsigned : bool;
  out_of_range : out_of_range_behavior;
} [@@deriving show]
type float_type = Common.float_type
let pp_float_type = Common.pp_float_type
type undefined_transform = Undefined | Null | EmptyString [@@deriving show]
type string_behavior = {
  null_as_empty_string : bool;
  undefined_as : undefined_transform;
} [@@deriving show]
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
  | SequenceType of types [@@deriving show]
type value = Common.value
let pp_value = Common.pp_value

type argument_kind = Single | Multiple | Optional | Default of value [@@deriving show]
type argument = { name : string; types : types; kind : argument_kind; user_attributes: user_attribute list } [@@deriving show]
and user_attribute =
  | UAPlain of string
  | UAEquals of string * string
  | UAArguments of string * argument list
  | UAArgumentsEquals of string * string * argument list [@@deriving show]

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
type constructor = { name : qualified_name; args : argument list; user_attributes: user_attribute list } [@@deriving show]
type interface = {
  inheritance: qualified_name list;
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
  user_attributes: user_attribute list
} [@@deriving show]
type dictionary_entry = {
  name : string;
  types : types;
  default_value : value option;
  user_attributes: user_attribute list
} [@@deriving show]
type dictionary = {
  name : qualified_name;
  inherits_from : qualified_name list;
  members : dictionary_entry list;
  user_attributes: user_attribute list
} [@@deriving show]
type exception_member = { name : string; types : types; user_attributes: user_attribute list } [@@deriving show]
type exception_ = {
  name : qualified_name;
  inherits_from : qualified_name list;
  consts : constant list;
  members : exception_member list;
  not_exposed : bool;
  user_attributes: user_attribute list
} [@@deriving show]
type enumerate = { name : qualified_name; values : string list; user_attributes: user_attribute list } [@@deriving show]
type callback = {
  name : qualified_name;
  return : types;
  args : argument list;
  treat_non_callable_as_null : bool;
  user_attributes: user_attribute list
} [@@deriving show]
let pp_qualified_map pp_content =
  let kvsep = Fmt.const Fmt.string ": "
  in let kvfmt = Fmt.pair ~sep:kvsep pp_qualified_name pp_content
  in let boxedkvfmt = Fmt.box ~indent:2 kvfmt
  in let fmtmap = Fmt.iter_bindings ~sep:Fmt.cut QNameMap.iter boxedkvfmt
  in Fmt.vbox ~indent:2 fmtmap
type global_constant = { name: qualified_name; types: types; value: value; user_attributes: user_attribute list } [@@deriving show]
type definitions = {
  dictionaries : dictionary QNameMap.t [@printer pp_qualified_map pp_dictionary];
  enumerations : enumerate QNameMap.t [@printer pp_qualified_map pp_enumerate];
  interfaces : interface QNameMap.t [@printer pp_qualified_map pp_interface];
  exceptions : exception_ QNameMap.t [@printer pp_qualified_map pp_exception_];
  callbacks : callback QNameMap.t [@printer pp_qualified_map pp_callback];
  callback_interfaces : interface QNameMap.t [@printer pp_qualified_map pp_interface];
  constants : global_constant QNameMap.t [@printer pp_qualified_map pp_global_constant];
  implements : (qualified_name * qualified_name) list;
} [@@deriving show]

