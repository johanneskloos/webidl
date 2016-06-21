open Common

type scoped_name = { absolute: bool; ends_in_domstring: bool; path: string list }

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

type extended_attribute =
    { name: string; equals: string option; arguments: arguments }
and extended_attribute_list = extended_attribute list
and 'a with_attributes = 'a * extended_attribute_list
and arguments = argument with_attributes list
and argument =
  | ArgOptional of { type_: type_; name: string; default: value option }
  | ArgRequired of { type_: type_; name: string; multiple: bool }

type const = { type_: type_; name: string; value: value }
type exception_field = { type_: type_; name: string }
type exception_member = EConst of const | EField of exception_field
type operation = {
  return_type: type_;
  name: string option;
  arguments: arguments;
  raises: scoped_name list
}

type qualifier =
    QStatic | QGetter | QSetter | QCreator | QDeleter | QLegacyCaller | QOmittable
type qualified_operation = qualifier option * operation
type get_mode = GRaises of scoped_name list | GInherits
type attribute = {
  inherited: bool;
  readonly: bool;
  type_: type_;
  name: string;
  get: get_mode;
  set: scoped_name list
}

type stringifier = StringBare | StringAttribute of attribute | StringOperation of operation
type attribute_or_operation =
    Stringifier of stringifier | Attribute of attribute | Operation of qualified_operation

type implements = scoped_name * scoped_name

type typedef = { name: string; type_: type_ with_attributes }

type callback = { name: string; type_: type_ with_attributes; arguments: arguments }

type enum = { name: string; contents: string list }

type inheritance = scoped_name list

type exception_members = exception_member with_attributes list
type exception_ = { name: string; inheritance: inheritance; members: exception_members }

type dictionary_member = { type_: type_; name: string; default: value option }
type dictionary_members = dictionary_member with_attributes list
type dictionary = { partial: bool; name: string; inheritance: inheritance; members: dictionary_members }

type interface_member =
  | IConst of const
  | IAttributeOrOperation of attribute_or_operation

type interface_members = interface_member with_attributes list
type regular_interface =
    { name: string; inheritance: inheritance; members: interface_members }
type partial_interface = { name: string; members: interface_members }
type interface =
  | IRegular of regular_interface
  | IPartial of partial_interface
  | IForward of string

type module_ = { name: string; definitions: definitions }
and definition =
  | DCallbackInterface of interface
  | DCallback of callback
  | DInterface of interface
  | DDictionary of dictionary
  | DException of exception_
  | DEnum of enum
  | DImplements of implements
  | DModule of module_
  | DTypedef of typedef
  | DConst of const
  | DNothing
and definitions = definition list
