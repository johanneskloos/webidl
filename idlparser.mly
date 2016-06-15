%{ open Ast %}
%token<string> Identifier String
%token Callback LBrace RBrace Semicolon Partial Interface Dictionary
%token Equals Exception Colon Enum LPar RPar Typedef Implements Const
%token True False Null Stringifier Inherit ReadOnly Attribute
%token Static Getter Setter Creator Deleter LegacyCaller Optional Ellipsis
%token LBracket RBracket (*Other Minus Dot*) Lt Gt Question Unrestricted Any OR
%token DOMString Sequence Object Date Boolean Byte Octet Float Double
%token Unsigned Short Long Void Comma EOF Module DoubleColon
%token<int> IntegerValue
%token<float> FloatValue
%start<(Ast.definitions * Ast.extended_attribute list) list> main
%type<(definitions * extended_attribute list) list> definitions
%type<definitions * extended_attribute list> single_definition
%type<definitions> callback_or_interface
%type<definitions> callback_rest_or_interface
%type<interface_data> interface
%type<definitions> partial
%type<definitions> partial_definition
%type<interface_data> partial_interface
%type<interface_member list> interface_members
%type<interface_member> interface_single_member
%type<members> interface_member
%type<dictionary_data> dictionary
%type<dictionary_member list> dictionary_members
%type<dictionary_member> single_dictionary_member
%type<dictionary_entry> dictionary_member
%type<dictionary_data> partial_dictionary
%type<value option> default
%type<value> default_value
%type<exception_data> exception_
%type<(exception_member * extended_attribute list) list> exception_members
%type<string list option> inheritance
%type<enum_data> enum
%type<typedef_data> typedef
%type<const_data> const
%type<value> const_value
%type<members> attribute_or_operation
%type<members> stringifier_attribute_or_operation
%type<attribute_data> attribute
%type<operation_data> operation
%type<qualifier list> qualifiers
%type<qualifier> special
%type<stringifier_operation_data> operation_rest
%type<argument_list> argument_list
%type<argument> argument
%type<argument_data> optional_or_required_argument
%type<string> argument_name
%type<exception_member> exception_member
%type<string> argument_name_keyword
%type<types> type_
%type<types> single_type
%type<types> union_type
%type<types> union_member_type
%type<types> non_any_type
%type<types> const_type
%type<base_types> primitive_type
%type<base_types> unrestricted_float_type
%type<base_types> float_type
%type<base_types> unsigned_integer_type
%type<base_types> integer_type
%type<types -> types> type_suffix
%type<types -> types> type_suffix_starting_with_array
%type<bool> null
%type<types> return_type
%type<string list> path

%%
main: defs=definitions EOF { defs }
definitions: defs=list(single_definition) { defs };
single_definition: 
      attrs=extended_attribute_list def=definition { (def, attrs) }
    | Module modname=Identifier LBrace defs=definitions RBrace Semicolon
      { (DefModule (modname, defs), []) }
definition:
      res=callback_or_interface { res }
    | res=partial { res }
    | dict=dictionary { DefDictionary dict }
    | ex=exception_ { DefException ex }
    | enum=enum { DefEnum enum }
    | typedef=typedef { DefTypedef typedef }
    | impl=implements_statement { DefImplements impl }
callback_or_interface:
      Callback coi=callback_rest_or_interface {
          match coi with
          | DefCallback cb -> DefCallback cb
          | DefInterface it -> DefCallbackInterface it
          | _ -> assert false
      }
    | it=interface { DefInterface it }
callback_rest_or_interface:
      cr=callback_rest { DefCallback cr}
    | it=interface { DefInterface it }
interface:
    Interface id=Identifier inh=inheritance LBrace mem=interface_members RBrace Semicolon
    { ([id], (match inh with Some id -> ModeInherit id | None -> ModeTop), mem) }
partial: Partial pd=partial_definition { pd }
partial_definition:
      pi=partial_interface { DefInterface pi }
    | pd=partial_dictionary { DefDictionary pd }
partial_interface:
      Interface id=Identifier LBrace mem=interface_members RBrace Semicolon
      { ([id], ModePartial, mem) }
interface_members: mem=list(interface_single_member) { mem }
interface_single_member: attr=extended_attribute_list mem=interface_member { (mem, attr) }
interface_member: const=const { ConstMember const } | attr=attribute_or_operation { attr }
dictionary: Dictionary id=Identifier inh=inheritance LBrace mem=dictionary_members RBrace Semicolon
    { ([id], (match inh with Some id -> ModeInherit id | None -> ModeTop), mem) }
dictionary_members: res=list(single_dictionary_member) { res }
single_dictionary_member: attr=extended_attribute_list mem=dictionary_member { (mem, attr) }
dictionary_member: ty=type_ id=Identifier def=default Semicolon { (id, ty, def) }
partial_dictionary: Dictionary id=Identifier LBrace mem=dictionary_members RBrace Semicolon
      { ([id], ModePartial, mem) }
default: res=option(Equals value=default_value { value }) { res }
default_value: value=const_value { value } | str=String { StringValue str }
exception_: Exception id=Identifier inh=inheritance LBrace mem=exception_members RBrace Semicolon
      { ([id], inh, mem) }
exception_members: res=list(attr=extended_attribute_list mem=exception_member { (mem, attr) }) { res }
inheritance: res=option(Colon id=path { id }) { res }
enum: Enum id=Identifier LBrace fields=separated_nonempty_list(Comma, String) RBrace Semicolon { ([id], fields) }
callback_rest: id=Identifier Equals ty=return_type LPar arg=argument_list RPar Semicolon { ([id], ty, arg) }
typedef: Typedef attrs=extended_attribute_list ty=type_ id=Identifier Semicolon { ([id], ty, attrs) }
implements_statement: id1=path Implements id2=path Semicolon { (id1, id2) }
const: Const ty=const_type id=Identifier Equals value=const_value Semicolon { (id, ty, value) }
const_value:
      True { BoolValue true }
    | False { BoolValue false }
    | value=FloatValue { FloatValue value }
    | value=IntegerValue { IntValue value }
    | Null { NullValue }
attribute_or_operation:
    Stringifier mem=stringifier_attribute_or_operation { mem }
  | attr=attribute { AttributeMember attr }
  | oper=operation { OperationMember oper }
stringifier_attribute_or_operation:
    attr=attribute { StringifierAttributeMember attr }
  | oper=operation_rest { StringifierOperationMember oper }
  | Semicolon { StringifierEmptyMember }
attribute:
    inh=option(Inherit) ro=option(ReadOnly)
    Attribute ty=type_ id=Identifier Semicolon
    { (id,
        (match inh with Some _ -> true | None -> false),
        (match ro with Some _ -> true | None -> false), ty) }
operation: qual=qualifiers oper=operation_rest
    { let (id, ty, arg) = oper in (id, ty, arg, qual) }
qualifiers: Static { [ SpecStatic ] } | res=list(special) { res }
special:
      | Getter { SpecGetter }
      | Setter { SpecSetter }
      | Creator { SpecCreator }
      | Deleter { SpecDeleter }
      | LegacyCaller { SpecLegacyCaller }
operation_rest: ty=return_type id=option(Identifier) LPar arg=argument_list RPar Semicolon
    { ((match id with Some id -> id | None -> ""), ty, arg) }
argument_list: res=separated_list(Comma, argument) { res }
argument: attrs=extended_attribute_list arg=optional_or_required_argument { (arg, attrs) }
optional_or_required_argument:
      Optional ty=type_ name=argument_name def=default
      { (name, ty, ModeOptional, def) }
    | ty=type_ mult=option(Ellipsis) name=argument_name
      { (name, ty, (match mult with Some _ -> ModeMultiple | None -> ModeSingle), None) }
argument_name: name=argument_name_keyword { name } | name=Identifier { name }
exception_member:
      const=const { ExConstMember const }
    | ty=type_ id=Identifier Semicolon { ExValueMember(id, ty) }
extended_attribute_list:
    res=option(LBracket attrs=separated_list(Comma, extended_attribute) RBracket { attrs })
    { match res with Some res -> res | None -> [] }
extended_attribute:
      res=extended_attribute_no_args { res }
    | res=extended_attribute_arg_list { res }
    | res=extended_attribute_ident { res }
    | res=extended_attribute_named_arg_list { res }

argument_name_keyword:
      Attribute { "attribute" }
    | Callback { "callback" }
    | Const { "const" }
    | Creator { "creator" }
    | Deleter { "deleter" }
    | Dictionary { "dictionary" }
    | Enum { "enum" }
    | Exception { "exception" }
    | Getter { "getter" }
    | Implements { "implements" }
    | Inherit { "inherit" }
    | Interface { "interface" }
    | LegacyCaller { "legacycaller" }
    | Partial { "partial" }
    | Setter { "setter" }
    | Stringifier { "stringifier" }
    | Typedef { "typedef" }
    | Unrestricted { "unrestricted" }

type_:
      ty=single_type { ty }
    | ut=union_type suff=type_suffix { suff ut }
single_type:
    | ty=non_any_type { ty }
    | Any suff=type_suffix_starting_with_array
      { suff (TypeLeaf AnyType) }
union_type: LPar ty=separated_nonempty_list(OR, union_member_type) RPar
      { TypeUnion ty }
union_member_type:
      ty=non_any_type { ty }
    | ut=union_type suff=type_suffix { suff ut }
    | Any LBracket RBracket suff=type_suffix
      { suff (TypeArray (TypeLeaf AnyType)) }
non_any_type:
    ty=primitive_type suff=type_suffix { suff (TypeLeaf ty) }
    | DOMString suff=type_suffix { suff (TypeLeaf DOMStringType) }
    | id=path suff=type_suffix { suff (TypeLeaf (NamedType id)) }
    | Sequence Lt ty=type_ Gt null=null
      { if null then TypeNullable (TypeSequence ty) else TypeSequence ty }
    | Object suff=type_suffix { suff (TypeLeaf ObjectType) }
    | Date suff=type_suffix { suff (TypeLeaf DateType) }

const_type:
      ty=primitive_type null=null
      { if null then TypeNullable (TypeLeaf ty) else (TypeLeaf ty) }
    | id=path null=null
      { if null then TypeNullable (TypeLeaf (NamedType id)) else TypeLeaf (NamedType id) }
primitive_type:
      ty=unsigned_integer_type { ty }
    | ty=unrestricted_float_type { ty }
    | Boolean { BooleanType }
    | Byte { ByteType }
    | Octet { OctetType }
unrestricted_float_type:
      Unrestricted ft=float_type
      { match ft with
        | FloatType -> UnrestrictedFloatType 
        | DoubleType -> UnrestrictedDoubleType
        | _ -> assert false }
   | ft=float_type { ft }
float_type: Float { FloatType } | Double { DoubleType }
unsigned_integer_type:
      Unsigned ty=integer_type
      { match ty with
        | ShortType -> UnsignedShortType
        | LongType -> UnsignedLongType
        | LongLongType -> UnsignedLongLongType
        | _ -> assert false }
    | ty=integer_type { ty }
integer_type: Short { ShortType } | Long { LongType } | Long Long { LongLongType }
type_suffix:
      LBracket RBracket suff=type_suffix
      { fun ty -> suff (TypeArray ty) }
    | Question suff=type_suffix_starting_with_array
      { fun ty -> suff (TypeOption ty) }
    | { fun ty -> ty }
type_suffix_starting_with_array:
      LBracket RBracket suff=type_suffix
      { fun ty -> suff (TypeArray ty) }
    | { fun ty -> ty }
null: res=option(Question) { match res with Some _ -> true | None -> false }
return_type: ty=type_ { ty } | Void { TypeLeaf VoidType }
extended_attribute_no_args: id=Identifier { WithoutArguments (id, None) }
extended_attribute_arg_list: id=Identifier LPar args=argument_list RPar { WithArguments (id, None, args) }
extended_attribute_ident: id=Identifier Equals id2=Identifier { WithoutArguments (id, Some id2) }
extended_attribute_named_arg_list:
    id=Identifier Equals id2=Identifier LPar args=argument_list RPar
    { WithArguments (id, Some id2, args) }
path: path=separated_nonempty_list(DoubleColon, id=Identifier {id}) {path}
