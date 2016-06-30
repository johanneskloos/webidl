module QualifiedName = struct
  type t = string list [@@deriving ord,eq]
end
type qualified_name = QualifiedName.t
let compare_qualified_name = QualifiedName.compare
let equal_qualified_name = QualifiedName.equal
let pp_qualified_name =
  let open Fmt in
    hbox (list ~sep:(const string "::") string)
module QNameMap = BatMap.Make(QualifiedName)

type out_of_range_behavior = Modulo | Clamp | Exception [@@deriving show]
type int_type = {
  length : Common.int_length;
  unsigned : bool;
  out_of_range : out_of_range_behavior;
}
let pp_int_type pp { length; unsigned; out_of_range } =
  let open Fmt in
  begin match out_of_range with
    | Modulo -> ()
    | Clamp -> string pp "《clamp》"
    | Exception -> string pp "《exception》"
  end;
  if unsigned then string pp "unsigned ";
  match length with
    | Common.IShort -> string pp "short"
    | Common.ILong -> string pp "long"
    | Common.ILongLong -> string pp "long long"

type float_type = Common.float_type
let pp_float_type = Common.pp_float_type
type undefined_transform = Undefined | Null | EmptyString [@@deriving show]
type string_behavior = {
  null_as_empty_string : bool;
  undefined_as : undefined_transform;
}
let pp_string_behavior pp { null_as_empty_string; undefined_as } =
  let open Fmt in
  if null_as_empty_string then string pp "《null↦̈\"\"》";
  match undefined_as with
    | Undefined -> ()
    | Null -> string pp "《undefined↦null》"
    | EmptyString -> string pp "《undefined↦\"\"》"

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
let rec pp_types pp =
  let open Fmt in function
      IntType it -> pp_int_type pp it
    | FloatType ft -> pp_float_type pp ft
    | NamedType sl -> list ~sep:(const string "::") string pp sl
    | AnyType -> string pp "any"
    | VoidType -> string pp "void"
    | DOMStringType sb -> pp_string_behavior pp sb; string pp "string"
    | DateType -> string pp "Date"
    | ObjectType -> string pp "object"
    | OctetType -> string pp "octet"
    | ByteType -> string pp "byte"
    | BooleanType -> string pp "boolean"
    | UnionType ts -> parens (list ~sep:(const string " or ") pp_types) pp ts
    | ArrayType t -> pp_types pp t; string pp "[]"
    | OptionType (undef, t) ->
        if undef then string pp "optional(undef-stuff)" else string pp "optional";
        pp_types pp t
    | NullableType t -> pp_types pp t; string pp "?"
    | SequenceType t -> string pp "sequence<"; pp_types pp t; string pp ">"

type value = Common.value
let pp_value = Common.pp_value

type argument_kind = Single | Multiple | Optional | Default of value [@@deriving show]
type argument = { name : string; types : types; kind : argument_kind; user_attributes: user_attribute list }
and user_attribute =
  | UAPlain of string
  | UAEquals of string * string
  | UAArguments of string * argument list
  | UAArgumentsEquals of string * string * argument list [@@deriving show]

let rec pp_argument pp { name; types; kind; user_attributes } =
  let open Fmt in
    match kind with
      | Single ->
          pf pp "%a@ %a@ %s" pp_user_attributes user_attributes pp_types types name
      | Multiple ->
          pf pp "%a...@ %a@ %s" pp_user_attributes user_attributes pp_types types name
      | Optional ->
          pf pp "optional %a@ %a@ %s" pp_user_attributes user_attributes pp_types types name
      | Default v ->
          pf pp "optional %a@ %a@ %s = %a" pp_user_attributes user_attributes pp_types types name pp_value v
and pp_user_attribute pp = let open Fmt in function
  | UAPlain name -> pf pp "%s" name
  | UAEquals (name, equ) -> pf pp "%s=%s" name equ
  | UAArguments (name, args) -> pf pp "%s%a" name pp_arguments args
  | UAArgumentsEquals (name, equ, args) -> pf pp "%s=%s(%a)" name equ pp_arguments args
and pp_arguments pp args =
  let open Fmt in
    parens (box (list ~sep:(prefix (const string ",") cut) pp_argument)) pp args
and pp_user_attributes pp args =
  let open Fmt in
    if args <> [] then
      brackets (box (list ~sep:(prefix (const string ",") cut) pp_user_attribute)) pp args

type special_handling = {
  this_lenient : bool;
  this_implicit : bool;
  named_properties_object : bool;
  override_builtins : bool;
} [@@deriving show]
type constant = { name : string; types : types; value : value; user_attributes: user_attribute list }
let pp_constant pp { name; types; value; user_attributes } =
  Fmt.pf pp "@[<hov>%a@ %a@ %s = %a@]" pp_user_attributes user_attributes pp_types types name pp_value value
type access =
    ReadWrite
  | ReadOnly
  | PutForwards of string
  | Replacable
  | Unforgable
let pp_access pp = let open Fmt in function
  | ReadWrite -> ()
  | ReadOnly -> string pp "《readonly》"
  | Replacable -> string pp "《replacable》"
  | Unforgable -> string pp "《unforgable》"
  | PutForwards fwto -> pf pp "《forwards: %s》" fwto

type attribute = {
  name : string;
  types : types;
  lenient_this : bool;
  inherited : bool;
  access : access;
  user_attributes: user_attribute list
}
let pp_attribute pp { name; types; lenient_this; inherited; access; user_attributes } =
  Fmt.pf pp "@[<hov>%a@ %a %a@ %s@]"
    pp_user_attributes user_attributes
    pp_access access
    pp_types types
    name
type operation = { name : string; return : types; args : argument list; user_attributes: user_attribute list }
let pp_operation pp op =
  Fmt.pf pp "@[<hov>%a@ %a@ %s%a@]"
    pp_user_attributes op.user_attributes
    pp_types op.return
    op.name
    pp_arguments op.args

type legacy_caller = { return : types; args : argument list; user_attributes: user_attribute list }
let pp_legacy_caller pp op =
  Fmt.pf pp "@[<hov>%a@ %a@ %a@]"
    pp_user_attributes op.user_attributes
    pp_types op.return
    pp_arguments op.args

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
type constructor = { name : qualified_name; args : argument list; user_attributes: user_attribute list }
let pp_constructor pp c =
  Fmt.pf pp "@[<hov>%a@ %a%a@]"
    pp_user_attributes c.user_attributes
    pp_qualified_name c.name
    pp_arguments c.args

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
}
let pp_sem pp x = Fmt.suffix (Fmt.const Fmt.string ";") pp x
let list_sem (pp: 'a Fmt.t): 'a list Fmt.t = Fmt.list (pp_sem pp)

let pp_interface pp i =
  let open Fmt in
  Fmt.pf pp "@[<v2>@[<hov>%a@ {@]@ " pp_qualified_name i.name;
  if (i.inheritance <> []) then
    Fmt.pf pp "Inherits from: @[<hov>%a@]@ " (list_sem pp_qualified_name) i.inheritance;
  if (i.consts <> []) then
    Fmt.pf pp "@[<v>Consts:@ %a@]@ " (list_sem pp_constant) i.consts;
  if (i.attributes <> []) then
    Fmt.pf pp "@[<v>Attributes:@ %a@]@ " (list_sem pp_attribute) i.attributes;
  if (i.operations <> []) then
    Fmt.pf pp "@[<v>Operations:@ %a@]@ " (list_sem pp_operation) i.operations;
  if (i.static_operations <> []) then
    Fmt.pf pp "@[<v>Static operations:@ %a@]@ " (list_sem pp_operation) i.static_operations;
  if (i.constructors <> []) then
    Fmt.pf pp "@[<v>Constructors:@ %a@]@ " (list_sem pp_constructor) i.constructors;
  if (i.legacy_callers <> []) then
    Fmt.pf pp "@[<v>Legacy callers:@ %a@]" (list_sem pp_legacy_caller) i.legacy_callers;
  Fmt.pf pp "@]@ }@ "

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
type exception_member = { name : string; types : types; user_attributes: user_attribute list }
let pp_exception_member pp { name; types; user_attributes } =
  Fmt.pf pp "%a@ %a %s"
    pp_user_attributes user_attributes
    pp_types types
    name

type exception_ = {
  name : qualified_name;
  inherits_from : qualified_name list;
  consts : constant list;
  members : exception_member list;
  not_exposed : bool;
  user_attributes: user_attribute list
}
let pp_exception_ pp { name; inherits_from; consts; members; not_exposed; user_attributes } =
  let open Fmt in
  Fmt.pf pp "@[<v2>%a%s@ " pp_qualified_name name (if not_exposed then " (not exposed)" else "");
  if user_attributes <> [] then
    Fmt.pf pp "@[<hov2>Attributes: %a@]" (list pp_user_attribute) user_attributes;
  if inherits_from <> [] then
    Fmt.pf pp "@[<hov2>Inherits from: %a@]" (list pp_qualified_name) inherits_from;
  if members <> [] then
    Fmt.pf pp "@[<hov2>Fields: %a@]" (list pp_exception_member) members;
  if consts <> [] then
    Fmt.pf pp "@[<hov2>Constants: %a@]" (list pp_constant) consts;
  Fmt.pf pp "@]@ "
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
type global_constant = { name: qualified_name; types: types; value: value; user_attributes: user_attribute list }
let pp_global_constant pp { name; types; value; user_attributes } =
  let open Fmt in
    pf pp "@[<hov>%a@ %a@ %a =@ %a;@]@ "
      pp_user_attributes user_attributes
      pp_types types
      pp_qualified_name name
      pp_value value

type definitions = {
  dictionaries : dictionary QNameMap.t;
  enumerations : enumerate QNameMap.t;
  interfaces : interface QNameMap.t;
  exceptions : exception_ QNameMap.t;
  callbacks : callback QNameMap.t;
  callback_interfaces : interface QNameMap.t;
  constants : global_constant QNameMap.t;
  implements : (qualified_name * qualified_name) list;
}
let pp_qname_values pp_content =
  let kvfmt = Fmt.using snd pp_content
  in let boxedkvfmt = Fmt.box ~indent:2 kvfmt
  in let fmtmap = Fmt.iter_bindings ~sep:Fmt.cut QNameMap.iter boxedkvfmt
  in Fmt.vbox ~indent:2 fmtmap

let pp_definitions pp defs =
  let open Fmt in
    pf pp "@[<v>";
    if not (QNameMap.is_empty defs.dictionaries) then
      pf pp "Dictionaries:@ %a@ " (pp_qname_values pp_dictionary) defs.dictionaries;
    if not (QNameMap.is_empty defs.enumerations) then
      pf pp "Enumerations:@ %a@ " (pp_qname_values pp_enumerate) defs.enumerations;
    if not (QNameMap.is_empty defs.interfaces) then
      pf pp "Interfaces:@ %a@ " (pp_qname_values pp_interface) defs.interfaces;
    if not (QNameMap.is_empty defs.callback_interfaces) then
      pf pp "Callback Interfaces:@ %a@ " (pp_qname_values pp_interface) defs.callback_interfaces;
    if not (QNameMap.is_empty defs.exceptions) then
      pf pp "Exceptions:@ %a@ " (pp_qname_values pp_exception_) defs.exceptions;
    if not (QNameMap.is_empty defs.callbacks) then
      pf pp "Callbacks:@ %a@ " (pp_qname_values pp_callback) defs.callbacks;
    if not (QNameMap.is_empty defs.constants) then
      pf pp "Constants:@ %a@ " (pp_qname_values pp_global_constant) defs.constants;
    pf pp "@]"

