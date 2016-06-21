%{
open Ast
open Common
%}
%token<string> Identifier String
%token Callback LBrace RBrace Semicolon Partial Interface Dictionary
%token Equals Exception Colon Enum LPar RPar Typedef Implements Const
%token True False Null Stringifier Inherit ReadOnly Attribute
%token Static Getter Setter Creator Deleter LegacyCaller Optional Ellipsis
%token LBracket RBracket (*Other Minus Dot*) Lt Gt Question Unrestricted Any OR
%token DOMString Sequence Object Date Boolean Byte Octet Float Double
%token Unsigned Short Long Void Comma EOF Module DoubleColon Infinity NaN
%token GetRaises SetRaises Raises Minus Omittable Valuetype In Inherits
%token<int> IntegerValue
%token<float> FloatValue

%start<Ast.definitions> main

%type<string> argument_name
%type<extended_attribute list> extended_attribute_list
%type<extended_attribute> extended_attribute
%type<scoped_name list> scoped_name_list
%type<scoped_name> scoped_name_after_colon scoped_name
%type<arguments> arguments
%type<argument with_attributes> argument
%type<argument> optional_or_required_argument
%type<Common.int_type> integer_type
%type<Common.float_type> float_type
%type<Common.value> const_value
%type<type_> type_

%type<exception_> exception_
%type<dictionary> dictionary
%type<'a with_attributes list> list_with_attributes
%%
main: defs=definitions EOF { defs }

list_with_attributes(object_):
    res=list(attr=extended_attribute_list obj=object_ { (obj, attr) }) { res }
comma_separated_list(object_): res=separated_nonempty_list(Comma, res=object_ { res }) { res }

definitions: res=list(definition) { res };

definition:
    | Callback x=interface { DCallbackInterface x }
    | x=callback { DCallback x }
    | x=interface { DInterface x }
    | x=partial_interface { DInterface x }
    | x=dictionary { DDictionary x }
    | x=partial_dictionary { DDictionary x }
    | x=exception_ { DException x }
    | x=enum { DEnum x }
    | x=typedef { match x with Some x -> DTypedef x | None -> DNothing }
    | x=implements_statement { DImplements x }
    | x=module_ { DModule x }
    | x=valuetype { match x with Some x -> DTypedef x | None -> DNothing }
    | x=const { DConst x }

module_: Module name=Identifier LBrace definitions=definitions RBrace
      { { name; definitions } }

interface:
      Interface name=Identifier inheritance=inheritance
      LBrace members=interface_members RBrace Semicolon
      { IRegular { name; inheritance; members } }
    | Interface name=Identifier Semicolon
      { IForward name }

partial_interface:
      Partial Interface name=Identifier LBrace members=interface_members RBrace Semicolon
      { IPartial { name; members } }

interface_members: res=list_with_attributes(interface_member) { res }

interface_member:
    c=const { IConst c } | x=attribute_or_operation { IAttributeOrOperation x }

dictionary:
      Dictionary name=Identifier inheritance=inheritance
      LBrace members=dictionary_members RBrace Semicolon
      { { name; inheritance; members; partial = false } }

partial_dictionary:
      Partial Dictionary name=Identifier LBrace members=dictionary_members RBrace Semicolon
      { { name; members; inheritance = []; partial = true } }

dictionary_members: res=list_with_attributes(dictionary_member) { res }

dictionary_member: type_=type_ name=Identifier default=default Semicolon
      { { type_; name; default } }

default: res=option(Equals v=const_value { v }) { res }

exception_:
      Exception name=Identifier inheritance=inheritance LBrace members=exception_members
      RBrace
      { { name; inheritance; members } }

exception_members: res=list_with_attributes(exception_member) { res }

inheritance: res=loption(Colon inh=comma_separated_list(scoped_name) { inh }) { res }

enum: Enum name=Identifier LBrace contents=comma_separated_list(String) RBrace Semicolon
      { { name; contents } }

callback: Callback name=Identifier Equals type_=type_ arguments=arguments Semicolon
      { { name; type_=(type_, []); arguments } }

typedef:
    | Typedef attr=extended_attribute_list type_=type_ name=Identifier Semicolon
      { Some ({ name; type_=(type_, attr) }) }
    | Typedef extended_attribute_list type_ DOMString Semicolon
      { None }

valuetype:
    | Valuetype name=Identifier type_=type_ Semicolon
      { Some ({ name; type_=(type_, [])}) }
    | Valuetype DOMString Sequence Lt Unsigned Short Gt Semicolon
      { None }

implements_statement: l=scoped_name Implements r=scoped_name Semicolon { (l, r) }

attribute_or_operation:
    | Stringifier s=stringifier_attribute_or_operation { Stringifier s }
    | a=attribute { Attribute a }
    | o=operation { Operation o }

stringifier_attribute_or_operation:
    | a=attribute { StringAttribute a }
    | o=operation_rest { StringOperation o }
    | Semicolon { StringBare }

attribute:
    inherited=boption(Inherit)
    readonly=boption(ReadOnly)
    Attribute type_=type_ name=Identifier get=get set=setraises Semicolon
    { { inherited; readonly; type_; name; get; set } }

get:
    | GetRaises LBrace names=scoped_name_list RBrace { GRaises names }
    | Inherits Getter { GInherits }
    | { GRaises [] }
setraises: res=loption(SetRaises LBrace names=scoped_name_list RBrace { names }) { res }

operation: q=option(qualifiers) o=operation_rest { (q, o) }
qualifiers:
    | Static { QStatic }
    | Getter { QGetter }
    | Setter { QSetter }
    | Creator { QCreator }
    | Deleter { QDeleter }
    | LegacyCaller { QLegacyCaller }
    | Omittable { QOmittable }

operation_rest:
    return_type=type_ name=option(Identifier) arguments=arguments
    raises=loption(Raises LBrace s=scoped_name_list RBrace { s }) Semicolon
    { ({ return_type; name; arguments; raises }: operation) }

exception_member:
    | c=const { EConst c }
    | type_=type_ name=Identifier Semicolon { EField { type_; name } }

const: Const type_=type_ name=Identifier Equals value=const_value Semicolon
        { { type_; name; value } }

const_value:
    | True { VBool true }
    | False { VBool false }
    | x=IntegerValue { VInt x }
    | x=FloatValue { VFloat x }
    | Infinity { VFloat infinity }
    | Minus Infinity { VFloat neg_infinity }
    | NaN { VFloat nan }
    | Null { VNull }
    | x=String { VString x }

type_:
    | Any { TAny }
    | Boolean { TBoolean }
    | Byte { TByte }
    | Octet { TOctet }
    | t=integer_type { TInt t }
    | t=float_type { TFloat t }
    | DOMString { TString }
    | t=scoped_name { TNamed t }
    | Sequence Lt t=type_ Gt { TSequence t }
    | Object { TObject }
    | Date { TDate }
    | t=type_ LBracket RBracket { TArray t }
    | t=type_ Question { TOptional t }
    | Void { TVoid }
    | LBrace t=separated_nonempty_list(OR, type_) RBrace { TUnion t }

integer_type:
    | unsigned=boption(Unsigned) Short
    { { unsigned; length = IShort } }
    | unsigned=boption(Unsigned) Long
    { { unsigned; length = ILong } }
    | unsigned=boption(Unsigned) Long Long
    { { unsigned; length = ILongLong } }

float_type:
    | unrestricted=boption(Unrestricted) Float
    { { unrestricted; double = false } }
    | unrestricted=boption(Unrestricted) Double
    { { unrestricted; double = true } }

arguments: LPar args=comma_separated_list(argument) RPar { args }
argument: attr=extended_attribute_list option(In) arg=optional_or_required_argument
    { (arg, attr) }
optional_or_required_argument:
    | Optional type_=type_ name=argument_name default=default
      { ArgOptional { type_; name; default } }
    | type_=type_ multiple=boption(Ellipsis) name=argument_name
      { ArgRequired { type_; name; multiple } }

argument_name:
    | x = Identifier { x }

extended_attribute_list:
      attrs=loption(LBracket attrs=comma_separated_list(extended_attribute) RBracket { attrs })
      { attrs }

extended_attribute:
    name=Identifier equals=option(Equals equ=Identifier { equ }) arguments=loption(arguments)
    { { name; equals; arguments } }

scoped_name_list: res=comma_separated_list(scoped_name) { res }
scoped_name:
    | DoubleColon name=scoped_name_after_colon
    { { name with absolute = true } }
    | id=Identifier
    { { absolute = false; path = [id]; ends_in_domstring = false } }
    | id=Identifier DoubleColon name=scoped_name_after_colon
    { { name with path = id :: name.path } }
scoped_name_after_colon:
    | id=Identifier
    { { absolute = false; path = [id]; ends_in_domstring = false } }
    | id=Identifier DoubleColon name=scoped_name_after_colon
    { { name with path = id :: name.path } }
    | DOMString
    { { absolute = false; path = []; ends_in_domstring = true } }
