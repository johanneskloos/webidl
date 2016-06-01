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

type types =
  | TypeLeaf of base_types
  | TypeUnion of types list
  | TypeArray of types
  | TypeOption of types
  | TypeNullable of types
  | TypeSequence of types

type value =
  | StringValue of string
  | BoolValue of bool
  | FloatValue of float
  | IntValue of int
  | NullValue


type qualifier =
    SpecLegacyCaller | SpecGetter | SpecSetter | SpecStatic | SpecCreator | SpecDeleter
type argument_mode = ModeSingle | ModeMultiple | ModeOptional

type argument_data = string * types * argument_mode * value option
type argument = argument_data * extended_attribute list 
and extended_attribute = 
    WithArguments of string * string option * argument list
  | WithoutArguments of string * string option
type argument_list = argument list
type stringifier_operation_data = string * types * argument_list
type const_data = string * types * value
type attribute_data = string * bool * bool * types
type operation_data = string * types * argument_list * qualifier list
type members =
    StringifierEmptyMember
  | StringifierOperationMember of stringifier_operation_data
  | StringifierAttributeMember of attribute_data
  | OperationMember of operation_data
  | AttributeMember of attribute_data
  | ConstMember of const_data
type exception_member =
  | ExConstMember of const_data
  | ExValueMember of string * types
type mode = ModeTop | ModePartial | ModeInherit of string

type dictionary_entry = string * types * value option
type dictionary_member = dictionary_entry * extended_attribute list
type interface_member = members * extended_attribute list
type dictionary_data = string * mode * dictionary_member list
type interface_data = string * mode * interface_member list
type exception_data = string * string option * (exception_member * extended_attribute list) list
type enum_data = string * string list
type typedef_data = string * types * extended_attribute list
type callback_data = string * types * argument list
type implements_data = string * string
type definitions =
    DefDictionary of dictionary_data
  | DefEnum of enum_data
  | DefInterface of interface_data
  | DefException of exception_data
  | DefTypedef of typedef_data
  | DefImplements of implements_data
  | DefCallback of callback_data
  | DefCallbackInterface of interface_data
