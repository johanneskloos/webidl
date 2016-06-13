type base_types =
    ShortType
  | LongType
  | LongLongType
  | UnsignedShortType
  | UnsignedLongType
  | UnsignedLongLongType
  | FloatType
  | DoubleType
  | UnrestrictedFloatType
  | UnrestrictedDoubleType
  | DOMStringType
  | NamedType of string
  | AnyType
  | VoidType
  | OctetType
  | ByteType
  | BooleanType
  | DateType
  | ObjectType
  [@@deriving show]

type types =
  | TypeLeaf of base_types
  | TypeUnion of types list
  | TypeArray of types
  | TypeOption of types
  | TypeNullable of types
  | TypeSequence of types
  [@@deriving show]

type value =
  | StringValue of string
  | BoolValue of bool
  | FloatValue of float
  | IntValue of int
  | NullValue
  [@@deriving show]


type qualifier =
    SpecLegacyCaller | SpecGetter | SpecSetter | SpecStatic | SpecCreator | SpecDeleter
  [@@deriving show]
type argument_mode = ModeSingle | ModeMultiple | ModeOptional
  [@@deriving show]

type argument_data = string * types * argument_mode * value option
  [@@deriving show]
type argument = argument_data * extended_attribute list 
  [@@deriving show]
and extended_attribute = 
    WithArguments of string * string option * argument list
  | WithoutArguments of string * string option
  [@@deriving show]
type argument_list = argument list
  [@@deriving show]
type stringifier_operation_data = string * types * argument_list
  [@@deriving show]
type const_data = string * types * value
  [@@deriving show]
type attribute_data = string * bool * bool * types
  [@@deriving show]
type operation_data = string * types * argument_list * qualifier list
  [@@deriving show]
type members =
    StringifierEmptyMember
  | StringifierOperationMember of stringifier_operation_data
  | StringifierAttributeMember of attribute_data
  | OperationMember of operation_data
  | AttributeMember of attribute_data
  | ConstMember of const_data
  [@@deriving show]
type exception_member =
  | ExConstMember of const_data
  | ExValueMember of string * types
  [@@deriving show]
type mode = ModeTop | ModePartial | ModeInherit of string
  [@@deriving show]

type dictionary_entry = string * types * value option
  [@@deriving show]
type dictionary_member = dictionary_entry * extended_attribute list
  [@@deriving show]
type interface_member = members * extended_attribute list
  [@@deriving show]
type dictionary_data = string * mode * dictionary_member list
  [@@deriving show]
type interface_data = string * mode * interface_member list
  [@@deriving show]
type exception_data = string * string option * (exception_member * extended_attribute list) list
  [@@deriving show]
type enum_data = string * string list
  [@@deriving show]
type typedef_data = string * types * extended_attribute list
  [@@deriving show]
type callback_data = string * types * argument list
  [@@deriving show]
type implements_data = string * string
  [@@deriving show]
type definitions =
    DefDictionary of dictionary_data
  | DefEnum of enum_data
  | DefInterface of interface_data
  | DefException of exception_data
  | DefTypedef of typedef_data
  | DefImplements of implements_data
  | DefCallback of callback_data
  | DefCallbackInterface of interface_data
  [@@deriving show]
type definition_list = (definitions * extended_attribute list) list [@@deriving show]

