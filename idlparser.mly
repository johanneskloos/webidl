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
%token GetRaises SetRaises Raises Minus Omittable Valuetype In Inherits InOut
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
    | a=extended_attribute_list Callback x=interface { DCallbackInterface (x, a) }
    | a=extended_attribute_list x=callback { DCallback (x, a) }
    | a=extended_attribute_list x=interface { DInterface (x, a) }
    | a=extended_attribute_list x=partial_interface { DInterface (x, a) }
    | a=extended_attribute_list x=dictionary { DDictionary (x, a) }
    | a=extended_attribute_list x=partial_dictionary { DDictionary (x, a) }
    | a=extended_attribute_list x=exception_ { DException (x, a) }
    | a=extended_attribute_list x=enum { DEnum (x, a) }
    | x=typedef { match x with Some x -> DTypedef x | None -> DNothing }
    | x=implements_statement { DImplements x }
    | x=module_ { DModule x }
    | x=valuetype { match x with Some x -> DTypedef x | None -> DNothing }
    | x=const { DConst x }

module_: Module name=identifier LBrace definitions=definitions RBrace Semicolon
      { { name; definitions } }

interface:
      Interface name=identifier inheritance=inheritance
      LBrace members=interface_members RBrace Semicolon
      { IRegular { name; inheritance; members } }
    | Interface name=identifier Semicolon
      { IForward name }

partial_interface:
      Partial Interface name=identifier LBrace members=interface_members RBrace Semicolon
      { IPartial { name; members } }

interface_members: res=list_with_attributes(interface_member) { res }

interface_member:
    c=const { IConst c } | x=attribute_or_operation { IAttributeOrOperation x }

dictionary:
      Dictionary name=identifier inheritance=inheritance
      LBrace members=dictionary_members RBrace Semicolon
      { { name; inheritance; members; partial = false } }

partial_dictionary:
      Partial Dictionary name=identifier LBrace members=dictionary_members RBrace Semicolon
      { { name; members; inheritance = []; partial = true } }

dictionary_members: res=list_with_attributes(dictionary_member) { res }

dictionary_member: type_=type_ name=identifier default=default Semicolon
      { { type_; name; default } }

default: res=option(Equals v=const_value { v }) { res }

exception_:
      Exception name=identifier inheritance=inheritance LBrace members=exception_members
      RBrace Semicolon
      { { name; inheritance; members } }

exception_members: res=list_with_attributes(exception_member) { res }

inheritance: res=loption(Colon inh=comma_separated_list(scoped_name) { inh }) { res }

enum: Enum name=identifier LBrace contents=comma_separated_list(String) RBrace Semicolon
      { { name; contents } }

callback: Callback name=identifier Equals type_=type_ arguments=arguments Semicolon
      { { name; type_=(type_, []); arguments } }

typedef:
    | Typedef attr=extended_attribute_list type_=type_ name=identifier Semicolon
      { Some ({ name; type_=(type_, attr) }) }
    | Typedef extended_attribute_list type_ DOMString Semicolon
      { None }

valuetype:
    | Valuetype name=identifier type_=type_ Semicolon
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
    Attribute type_=type_ name=argument_name get=get set=setraises Semicolon
    { { inherited; readonly; type_; name; get; set } }

get:
    | GetRaises LBrace names=scoped_name_list RBrace { GRaises names }
    | Inherits Getter { GInherits }
    | { GRaises [] }
setraises: res=loption(SetRaises LPar names=scoped_name_list RPar { names }) { res }

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
    return_type=type_ name=option(identifier) arguments=arguments
    raises=loption(Raises LPar s=scoped_name_list RPar { s }) Semicolon
    { ({ return_type; name; arguments; raises }: operation) }

exception_member:
    | c=const { EConst c }
    | type_=type_ name=identifier Semicolon { EField { type_; name } }

const: Const type_=type_ name=identifier Equals value=const_value Semicolon
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

argmode: In | InOut | { () }
arguments: LPar args=separated_list(Comma, arg=argument { arg }) RPar { args }
argument: attr=extended_attribute_list argmode arg=optional_or_required_argument
    { (arg, attr) }
optional_or_required_argument:
    | Optional type_=type_ name=argument_name default=default
      { ArgOptional { type_; name; default } }
    | type_=type_ multiple=boption(Ellipsis) name=argument_name
      { ArgRequired { type_; name; multiple } }

argument_name:
    | x = identifier { x }
    | Callback { "callback" }
    | Partial { "partial" }
    | Dictionary { "dictionary" }
    | Exception { "exception" }
    | Enum { "enum" }
    | Attribute { "attribute" }
    | Module { "module" }
    | Valuetype { "valuetype" }
    | Object { "object" }

extended_attribute_list:
      attrs=loption(LBracket attrs=comma_separated_list(extended_attribute) option(Comma) RBracket { attrs })
      { attrs }

extended_attribute:
    name=identifier equals=option(Equals equ=identifier { equ }) arguments=option(arguments)
    { { name; equals; arguments } }

scoped_name_list: res=comma_separated_list(scoped_name) { res }
scoped_name:
    | DoubleColon name=scoped_name_after_colon
    { { name with absolute = true } }
    | id=identifier
    { { absolute = false; path = [id]; ends_in_domstring = false } }
    | id=identifier DoubleColon name=scoped_name_after_colon
    { { name with path = id :: name.path } }
scoped_name_after_colon:
    | id=identifier
    { { absolute = false; path = [id]; ends_in_domstring = false } }
    | id=identifier DoubleColon name=scoped_name_after_colon
    { { name with path = id :: name.path } }
    | DOMString
    { { absolute = false; path = []; ends_in_domstring = true } }

identifier:
    id=Identifier { id }

