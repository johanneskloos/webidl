open Common

type scoped_name = { absolute: bool; ends_in_domstring: bool; path: string list }
                     [@@deriving show]

type type_ =
  | TBoolean
  | TByte
  | TOctet
  | TInt of Common.int_type
  | TFloat of Common.float_type
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

type qualifier =
    QStatic | QGetter | QSetter | QCreator | QDeleter | QLegacyCaller | QOmittable
  [@@deriving show]
type qualified_operation = qualifier option * operation
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
type attribute_or_operation =
    Stringifier of stringifier | Attribute of attribute | Operation of qualified_operation
  [@@deriving show]

type implements = scoped_name * scoped_name
  [@@deriving show]

type typedef = { name: string; type_: type_ with_attributes }
  [@@deriving show]

type callback = { name: string; type_: type_ with_attributes; arguments: arguments }
  [@@deriving show]

type enum = { name: string; contents: string list }
  [@@deriving show]

type inheritance = scoped_name list
  [@@deriving show]

type exception_members = exception_member with_attributes list
  [@@deriving show]
type exception_ = { name: string; inheritance: inheritance; members: exception_members }
  [@@deriving show]

type dictionary_member = { type_: type_; name: string; default: value option }
  [@@deriving show]
type dictionary_members = dictionary_member with_attributes list
  [@@deriving show]
type dictionary = { partial: bool; name: string; inheritance: inheritance; members: dictionary_members }
  [@@deriving show]

type interface_member =
  | IConst of const
  | IAttributeOrOperation of attribute_or_operation
  [@@deriving show]

type interface_members = interface_member with_attributes list
  [@@deriving show]
type regular_interface =
    { name: string; inheritance: inheritance; members: interface_members }
  [@@deriving show]
type partial_interface = { name: string; members: interface_members }
  [@@deriving show]
type interface =
  | IRegular of regular_interface
  | IPartial of partial_interface
  | IForward of string
  [@@deriving show]

type module_ = { name: string; definitions: definitions }
  [@@deriving show]
and definition =
  | DCallbackInterface of interface with_attributes
  | DCallback of callback with_attributes
  | DInterface of interface with_attributes
  | DDictionary of dictionary with_attributes
  | DException of exception_ with_attributes
  | DEnum of enum with_attributes
  | DImplements of implements
  | DModule of module_
  | DTypedef of typedef
  | DConst of const
  | DNothing
  [@@deriving show]
and definitions = definition list
  [@@deriving show]
