open Common
open Ast

let pp_sconst s = Fmt.const Fmt.string s
let comma_sep = Fmt.suffix Fmt.cut (pp_sconst ",")

type scoped_name = NamePath of string list | NameBuiltin of string
let pp_scoped_name pp = function
  | NamePath l -> Fmt.list ~sep:(pp_sconst "::") Fmt.string pp (List.rev l)
  | NameBuiltin b -> Fmt.string pp b

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
let rec pp_type_ pp = function
  | TBoolean -> Fmt.string pp "boolean"
  | TByte -> Fmt.string pp "byte"
  | TOctet -> Fmt.string pp "ocat"
  | TInt d ->
      if d.unsigned then Fmt.string pp "unsigned ";
      begin match d.length with
        | IShort -> Fmt.string pp "short"
        | ILong -> Fmt.string pp "long"
        | ILongLong -> Fmt.string pp "long long"
      end
  | TFloat { unrestricted; double } ->
      if unrestricted then Fmt.string pp "unrestricted ";
      if double then Fmt.string pp "double" else Fmt.string pp "single"
  | TString -> Fmt.string pp "string"
  | TNamed n -> pp_scoped_name pp n
  | TSequence t -> Fmt.string pp "sequence<"; pp_type_ pp t; Fmt.string pp ">"
  | TObject -> Fmt.string pp "object"
  | TDate -> Fmt.string pp "Date"
  | TArray t -> pp_type_ pp t; Fmt.string pp "[]"
  | TOptional t -> pp_type_ pp t; Fmt.string pp "?"
  | TVoid -> Fmt.string pp "void"
  | TUnion ts -> Fmt.parens (Fmt.list ~sep:(pp_sconst " or ") pp_type_) pp ts
  | TAny -> Fmt.string pp "any"

type extended_attribute =
    { name: string; equals: string option; arguments: arguments option }
and extended_attribute_list = extended_attribute list
and 'a with_attributes = 'a * extended_attribute_list
and arguments = argument with_attributes list
and argument =
  | ArgOptional of { type_: type_; name: string; default: value option }
  | ArgRequired of { type_: type_; name: string; multiple: bool }

let rec fmt_extended_attribute pp { name; equals; arguments } =
  match equals, arguments with
    | None, None -> Fmt.string pp name
    | None, Some args -> Fmt.string pp name; pp_arguments pp args
    | Some equ, None -> Fmt.string pp name; Fmt.string pp "="; Fmt.string pp equ
    | Some equ, Some args  ->
        Fmt.string pp name; Fmt.string pp "="; Fmt.string pp equ;
        pp_arguments pp args
and pp_extended_attribute_list pp l =
  Fmt.list ~sep:comma_sep fmt_extended_attribute pp l; Fmt.cut pp ()
and pp_arguments pp =
  Fmt.parens (Fmt.list ~sep:comma_sep
    (fun pp (arg, attrs) ->
       if attrs <> [] then
         Fmt.brackets pp_extended_attribute_list pp attrs;
       pp_argument pp arg))
    pp
and pp_argument pp = function
  | ArgOptional { type_; name; default=None } ->
      Fmt.pf pp "optional %a %s" pp_type_ type_ name
  | ArgOptional { type_; name; default=Some v } ->
      Fmt.pf pp "optional %a %s = %a" pp_type_ type_ name pp_value v
  | ArgRequired { type_; name; multiple=false } ->
      Fmt.pf pp "%a %s" pp_type_ type_ name
  | ArgRequired { type_; name; multiple=true } ->
      Fmt.pf pp "%a... %s" pp_type_ type_ name

let pp_with_attributes pp_body pp (body, attrs) =
  if attrs <> [] then begin
    Fmt.brackets pp_extended_attribute_list pp attrs;
    Fmt.cut pp ()
  end;
  pp_body pp body

type global_const = { type_: type_; name: scoped_name; value: value }
let pp_global_const pp { type_; name; value } =
  Fmt.pf pp "const %a %a = %a;" pp_type_ type_ pp_scoped_name name pp_value value
type const = { type_: type_; name: string; value: value }
let pp_const pp { type_; name; value } =
  Fmt.pf pp "const %a %s = %a;" pp_type_ type_ name pp_value value
type exception_field = { type_: type_; name: string }
let pp_exception_field pp { type_; name } =
  Fmt.pf pp "%a %s;" pp_type_ type_ name
type exception_member = EConst of const | EField of exception_field
let pp_exception_member pp = function
  | EConst c -> pp_const pp c
  | EField f -> pp_exception_field pp f
type operation = {
  return_type: type_;
  name: string option;
  arguments: arguments;
  raises: scoped_name list
}
let pp_operation pp { return_type; name; arguments; raises } =
  Fmt.pf pp "@[<hov 2>%a %a%a" pp_type_ return_type
    (Fmt.option ~none:Fmt.nop Fmt.string) name pp_arguments arguments;
  if raises <> [] then
    Fmt.pf pp "@ raises (%a)" (Fmt.list ~sep:comma_sep pp_scoped_name) raises;
  Fmt.pf pp "@];"

type qualifier = Ast.qualifier =
  QStatic | QGetter | QSetter | QCreator | QDeleter | QLegacyCaller | QOmittable
let pp_qualifier pp = function
  | QStatic -> Fmt.string pp "static"
  | QGetter -> Fmt.string pp "getter"
  | QSetter -> Fmt.string pp "setter"
  | QCreator -> Fmt.string pp "creator"
  | QDeleter -> Fmt.string pp "deleter"
  | QLegacyCaller -> Fmt.string pp "legacycaller"
  | QOmittable -> Fmt.string pp "omittable"

type get_mode = GRaises of scoped_name list | GInherits
let pp_get_mode pp = function
  | GRaises [] -> ()
  | GRaises exc ->
      Fmt.pf pp "getraises (%a)@ "
        (Fmt.list ~sep:comma_sep pp_scoped_name) exc
  | GInherits -> Fmt.string pp "inherits getter@ "

type attribute = {
  inherited: bool;
  readonly: bool;
  type_: type_;
  name: string;
  get: get_mode;
  set: scoped_name list
}
let pp_attribute pp { inherited; readonly; type_; name; get; set } =
  Fmt.pf pp "@[<hov 2>";
  if inherited then Fmt.pf pp "inherited@ ";
  if readonly then Fmt.pf pp "readonly@ ";
  Fmt.pf pp "%a@ %s%a" pp_type_ type_ name pp_get_mode get;
  if set <> [] then
    Fmt.pf pp "setraises (%a)@ " (Fmt.list ~sep:comma_sep pp_scoped_name) set;
  Fmt.pf pp "@];"

type stringifier = StringBare | StringAttribute of attribute | StringOperation of operation
let pp_stringifier pp = function
  | StringBare -> Fmt.pf pp "stringifier;"
  | StringAttribute a -> Fmt.pf pp "stringifier %a" pp_attribute a
  | StringOperation o -> Fmt.pf pp "stringifier %a" pp_operation o

type implements = scoped_name * scoped_name
let pp_implements pp (l, r) =
  Fmt.pf pp "%a implements %a;@ " pp_scoped_name l pp_scoped_name r

type typedef = { name: scoped_name; type_: type_ with_attributes }
let pp_typedef pp { name; type_ } =
  Fmt.pf pp "typedef %a %a;@ " (pp_with_attributes pp_type_) type_ pp_scoped_name name

type callback = { name: scoped_name; type_: type_ with_attributes; arguments: arguments }
let pp_callback pp { name; type_; arguments } =
  Fmt.pf pp "@[<hov 2>callback %a %a%a@];@ "
    (pp_with_attributes pp_type_) type_
    pp_scoped_name name
    pp_arguments arguments

type enum = { name: scoped_name; contents: string list }
let pp_enum pp { name; contents } =
  Fmt.pf pp "@[<hov 2>enum %a { %a }@];@ "
    pp_scoped_name name
    (Fmt.list ~sep:comma_sep (fun pp s -> Fmt.pf pp "\"%s\"" s)) contents

type inheritance = scoped_name list
let pp_inheritance pp = function
  | [] -> ()
  | inh -> Fmt.pf pp ": @[<hov>%a@]" (Fmt.list ~sep:comma_sep pp_scoped_name) inh

type exception_members = exception_member with_attributes list
let pp_exception_members =
  Fmt.list ~sep:Fmt.cut (pp_with_attributes pp_exception_member)

type exception_ = { name: scoped_name; inheritance: inheritance; members: exception_members }
let pp_exception_ pp { name; inheritance; members } =
  Fmt.pf pp "@[<hov 2>exception %a%a@ {@ %a@ }@];@ "
    pp_scoped_name name
    pp_inheritance inheritance
    pp_exception_members members

type dictionary_member = { type_: type_; name: string; default: value option }
let pp_dictionary_member pp { type_; name; default } =
  Fmt.pf pp "@[<hov>%a %s" pp_type_ type_ name;
  begin match default with
    | None -> ()
    | Some v -> Fmt.pf pp " = %a" pp_value v
  end;
  Fmt.pf pp "@];@ "

type dictionary_members = dictionary_member with_attributes list
let pp_dictionary_members =
  Fmt.list ~sep:Fmt.cut (pp_with_attributes pp_dictionary_member)

type dictionary = { name: scoped_name; inheritance: inheritance; members: dictionary_members }
let pp_dictionary pp { name; inheritance; members } =
  Fmt.pf pp "@[<v 2>dictionary %a@[<hov>%a@] {@ %a}@];@ "
    pp_scoped_name name
    pp_inheritance inheritance
    pp_dictionary_members members

type interface_member =
  | IConst of const
  | IAttribute of attribute
  | IOperation of operation
  | ISpecialOperation of qualifier * operation
  | IStringifier of stringifier
let pp_interface_member pp = function
  | IConst x -> pp_const pp x
  | IAttribute x -> pp_attribute pp x
  | IOperation x -> pp_operation pp x
  | IStringifier x -> pp_stringifier pp x
  | ISpecialOperation (q, o) -> Fmt.pf pp "%a %a" pp_qualifier q pp_operation o


type interface_members = interface_member with_attributes list
let pp_interface_members = Fmt.list ~sep:Fmt.cut (pp_with_attributes pp_interface_member)
type interface = { name: scoped_name; inheritance: inheritance; members: interface_members }
let pp_interface pp { name; inheritance; members } =
  Fmt.pf pp "@[<v>interface %a@[<hov>%a@] {@ @[<v2>  %a@]@ }@];@ "
    pp_scoped_name name
    pp_inheritance inheritance
    pp_interface_members members

type definition =
  | DCallbackInterface of interface with_attributes
  | DCallback of callback with_attributes
  | DInterface of interface with_attributes
  | DDictionary of dictionary with_attributes
  | DException of exception_ with_attributes
  | DEnum of enum with_attributes
  | DImplements of implements
  | DConst of global_const
let pp_definition pp = function
  | DCallbackInterface x -> Fmt.string pp "callback "; pp_with_attributes pp_interface pp x
  | DCallback x -> pp_with_attributes pp_callback pp x
  | DInterface x -> pp_with_attributes pp_interface pp x
  | DDictionary x -> pp_with_attributes pp_dictionary pp x
  | DException x -> pp_with_attributes pp_exception_ pp x
  | DEnum x -> pp_with_attributes pp_enum pp x
  | DImplements x -> pp_implements pp x
  | DConst x -> pp_global_const pp x
type definitions = definition list
let pp_definitions = Fmt.vbox (Fmt.list ~sep:Fmt.cut pp_definition)
