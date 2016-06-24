open Common
open Ast

type scoped_name = NamePath of string list | NameBuiltin of string
  [@@deriving show]

type type_ =
  | TBoolean
  | TByte
  | TOctet
  | TInt of int_type
  | TFloat of float_type
  | TString
  | TNamed of scoped_name
  | TSequence of type_
  | TObject
  | TDate
  | TArray of type_
  | TOptional of type_
  | TVoid
  | TUnion of type_ list
  | TAny
  [@@deriving show]

type extended_attribute =
    { name: string; equals: string option; arguments: arguments option }
  [@@deriving show]
and extended_attribute_list = extended_attribute list
  [@@deriving show]
and 'a with_attributes = 'a * extended_attribute_list
  [@@deriving show]
and arguments = argument with_attributes list
  [@@deriving show]
and argument =
  | ArgOptional of { type_: type_; name: string; default: value option }
  | ArgRequired of { type_: type_; name: string; multiple: bool }
  [@@deriving show]

type global_const = { type_: type_; name: scoped_name; value: value }
  [@@deriving show]
type const = { type_: type_; name: string; value: value }
  [@@deriving show]
type exception_field = { type_: type_; name: string }
  [@@deriving show]
type exception_member = EConst of const | EField of exception_field
  [@@deriving show]
type operation = {
  return_type: type_;
  name: string option;
  arguments: arguments;
  raises: scoped_name list
}
  [@@deriving show]

type qualifier = Ast.qualifier =
    QStatic | QGetter | QSetter | QCreator | QDeleter | QLegacyCaller | QOmittable
  [@@deriving show]
type get_mode = GRaises of scoped_name list | GInherits
  [@@deriving show]
type attribute = {
  inherited: bool;
  readonly: bool;
  type_: type_;
  name: string;
  get: get_mode;
  set: scoped_name list
}
  [@@deriving show]

type stringifier = StringBare | StringAttribute of attribute | StringOperation of operation
  [@@deriving show]

type implements = scoped_name * scoped_name
  [@@deriving show]

type typedef = { name: scoped_name; type_: type_ with_attributes }
  [@@deriving show]

type callback = { name: scoped_name; type_: type_ with_attributes; arguments: arguments }
  [@@deriving show]

type enum = { name: scoped_name; contents: string list }
  [@@deriving show]

type inheritance = scoped_name list
  [@@deriving show]

type exception_members = exception_member with_attributes list
  [@@deriving show]
type exception_ = { name: scoped_name; inheritance: inheritance; members: exception_members }
  [@@deriving show]

type dictionary_member = { type_: type_; name: string; default: value option }
  [@@deriving show]
type dictionary_members = dictionary_member with_attributes list
  [@@deriving show]
type dictionary = { name: scoped_name; inheritance: inheritance; members: dictionary_members }
  [@@deriving show]

type interface_member =
  | IConst of const
  | IAttribute of attribute
  | IOperation of operation
  | ISpecialOperation of qualifier * operation
  | IStringifier of stringifier
  [@@deriving show]

type interface_members = interface_member with_attributes list
  [@@deriving show]
type interface = { name: scoped_name; inheritance: inheritance; members: interface_members }
  [@@deriving show]

type definition =
  | DCallbackInterface of interface with_attributes
  | DCallback of callback with_attributes
  | DInterface of interface with_attributes
  | DDictionary of dictionary with_attributes
  | DException of exception_ with_attributes
  | DEnum of enum with_attributes
  | DImplements of implements
  | DConst of global_const
  [@@deriving show]
type definitions = definition list
