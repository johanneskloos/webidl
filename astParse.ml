open IdlData
(** Second step: Translate the AST to a nicer form *)

type 'a arg_desc =
  | ArgPlain of string * ('a -> 'a)
  | ArgEquals of string * ('a -> string -> 'a)
  | ArgEqualsSpecific of string * (string * ('a -> 'a)) list
  | ArgArguments of string * ('a -> Ast.argument list -> 'a)
  | ArgEqualsAndArguments of string * ('a -> string -> Ast.argument list -> 'a)
  | ArgPlainOrEquals of string * ('a -> 'a) * ('a -> string -> 'a)
  | ArgPlainOrEqualsSpecific of string * ('a -> 'a) * (string * ('a -> 'a)) list
  | ArgArgumentsMaybeEquals of string * ('a -> string option -> Ast.argument list -> 'a)
  | ArgMaybeArguments of string * ('a -> Ast.argument list option -> 'a)
  | ArgMaybeArgumentsEquals of string * ('a -> string -> Ast.argument list option -> 'a)

let warn ctx str = prerr_endline str
let fail ctx str = failwith str

let parse_plain ctx name f state = function
  | Ast.WithoutArguments(name', None) when name=name' -> Some (f state)
  | Ast.WithoutArguments(name', _) when name=name' ->
      warn ctx "Argument given with '=value' expression it didn't expect"; Some (f state)
  | Ast.WithArguments(name', _, _) when name=name' ->
      warn ctx "Argument given with argument list it didn't expect"; Some (f state)
  | _ -> None
let parse_equals ctx name f state = function
  | Ast.WithoutArguments(name', None) when name=name' ->
      fail ctx "Argument given without expected '=value' expression"
  | Ast.WithoutArguments(name', Some id) when name=name' -> Some (f state id)
  | Ast.WithArguments(name', None, _) when name=name' ->
      fail ctx "Argument given without expected '=value' expression, but unexpected arglist"
  | Ast.WithArguments(name', Some id, _) when name=name' ->
      warn ctx "Argument given with argument list it didn't expect"; Some (f state id)
  | _ -> None
let parse_equals_specific ctx name fs state = function
  | Ast.WithoutArguments(name', Some id) when name=name' ->
      begin try
        Some (List.assoc id fs state)
      with Not_found ->
        fail ctx ("Argument given with unknown equality " ^ id)
      end
  | Ast.WithArguments(name', None, _) when name=name' ->
      fail ctx "Argument given without expected '=value' expression, but unexpected arglist"
  | Ast.WithArguments(name', Some id, _) when name=name' ->
      warn ctx "Argument given with argument list it didn't expect";
      begin try
        Some (List.assoc id fs state)
      with Not_found ->
        fail ctx ("Argument given with unknown equality " ^ id)
      end
  | _ -> None
let parse_arguments ctx name f state = function
  | Ast.WithoutArguments(name', _) when name=name' ->
      fail ctx "Argument given without expected argument list"
  | Ast.WithArguments(name', None, args) -> Some (f state args)
  | Ast.WithArguments(name', _, args) ->
      warn ctx "Argument given with '=value' it didn't expect"; Some (f state args)
  | _ -> None
let parse_equals_and_arguments ctx name f state = function
  | Ast.WithoutArguments(name', _) when name=name' ->
      fail ctx "Argument given without expected argument list"
  | Ast.WithArguments(name', None, _) when name=name' ->
      fail ctx "Argument given without expected '=value' expression"
  | Ast.WithArguments(name', Some id, args) when name=name' -> Some (f state id args)
  | _ -> None
let parse_plain_or_equals ctx name f fs state = function
  | Ast.WithoutArguments(name', None) when name=name' -> Some (f state)
  | Ast.WithoutArguments(name', Some id) when name=name' -> Some (fs state id)
  | Ast.WithArguments(name', None, _) when name=name' ->
      warn ctx "Argument given with argument list it didn't expect"; Some (f state)
  | Ast.WithArguments(name', Some id, _) when name=name' ->
      warn ctx "Argument given with argument list it didn't expect"; Some (fs state id)
  | _ -> None
let parse_plain_or_equals_specific ctx name f fs state = function
  | Ast.WithoutArguments(name', None) when name=name' -> Some (f state)
  | Ast.WithoutArguments(name', Some id) when name=name' ->
      begin try
        Some (List.assoc id fs state)
      with Not_found ->
        fail ctx ("Argument given with unknown equality " ^ id)
      end
  | Ast.WithArguments(name', None, _) when name=name' ->
      warn ctx "Argument given with argument list it didn't expect"; Some (f state)
  | Ast.WithArguments(name', Some id, _) when name=name' ->
      warn ctx "Argument given with argument list it didn't expect";
      begin try
        Some (List.assoc id fs state)
      with Not_found ->
        fail ctx ("Argument given with unknown equality " ^ id)
      end
  | _ -> None
let parse_arguments_maybe_equals ctx name f state = function
  | Ast.WithoutArguments(name', _) when name = name' ->
      fail ctx "Argument given without argument list"
  | Ast.WithArguments(name', id, args) when name=name' ->
      Some (f state id args)
  | _ -> None
let parse_maybe_arguments ctx name f state = function
  | Ast.WithoutArguments(name', None) when name=name' -> Some (f state None)
  | Ast.WithoutArguments(name', _) when name=name' ->
      warn ctx "Argument given with '=value' it didn't expect"; Some (f state None)
  | Ast.WithArguments(name', None, args) -> Some (f state (Some args))
  | Ast.WithArguments(name', _, args) ->
      warn ctx "Argument given with '=value' it didn't expect"; Some (f state (Some args))
  | _ -> None
let parse_maybe_arguments_equals ctx name f state = function
  | (Ast.WithoutArguments(name', None)|Ast.WithArguments(name', None, _)) when name=name' ->
      fail ctx "Required '=value' missing"
  | Ast.WithoutArguments(name', Some id) when name=name' -> Some (f state id None)
  | Ast.WithArguments(name', Some id, args) -> Some (f state id (Some args))
  | _ -> None

let parse_argument_cases ctx state = function
  | ArgPlain (name, f) -> parse_plain ctx name f state
  | ArgEquals (name, f) -> parse_equals ctx name f state
  | ArgEqualsSpecific (name, f) -> parse_equals_specific ctx name f state
  | ArgPlainOrEquals (name, f, fs) -> parse_plain_or_equals ctx name f fs state
  | ArgPlainOrEqualsSpecific (name, f, fs) ->
      parse_plain_or_equals_specific ctx name f fs state
  | ArgArguments (name, f) -> parse_arguments ctx name f state
  | ArgEqualsAndArguments (name, f) -> parse_equals_and_arguments ctx name f state
  | ArgArgumentsMaybeEquals (name, f) -> parse_arguments_maybe_equals ctx name f state
  | ArgMaybeArguments (name, f) -> parse_maybe_arguments ctx name f state
  | ArgMaybeArgumentsEquals (name, f) -> parse_maybe_arguments_equals ctx name f state

let parse_argument ctx state arg_desc arg =
  let rec apply_first = function
    | rule :: rest -> begin match parse_argument_cases ctx state rule arg with
        | Some state' -> Some state'
        | None -> apply_first rest
      end
    | [] -> None
  in apply_first arg_desc

let parse_all_arguments ctx init arg_desc =
  List.fold_left (fun state arg -> match parse_argument ctx state arg_desc arg with
                    | Some state' -> state'
                    | None ->
                        let name = match arg with
                            Ast.WithArguments (name, _, _)
                          | Ast.WithoutArguments (name, _) -> name
                        in warn ctx ("Unexpected argument " ^ name); state)
    init

let parse_some_arguments ctx init arg_desc =
  List.fold_left (fun (state, args) arg -> match parse_argument ctx state arg_desc arg with
                    | Some state' -> (state', args)
                    | None -> (state, arg :: args))
    (init, [])
let limit_arguments ctx arg_list =
  List.filter (function
                 | Ast.WithArguments (name, _, _)
                 | Ast.WithoutArguments (name, _) ->
                     if List.mem name arg_list then true else begin
                       warn ctx ("Removed non-permissible attribute " ^ name); false
                     end)
let forbid_arguments ctx arg_list =
  List.filter (function
                 | Ast.WithArguments (name, _, _)
                 | Ast.WithoutArguments (name, _) ->
                     if not (List.mem name arg_list) then true else begin
                       warn ctx ("Removed non-permissible attribute " ^ name); false
                     end)

let parse_int_args ctx args =
  parse_all_arguments ctx Modulo [
    ArgPlain("Clamp", function
               | Modulo -> Clamp
               | _ -> (warn ctx "Integer behavior given more than once"; Clamp));
    ArgPlain("EnforceRange", function
               | Modulo -> Exception
               | _ -> (warn ctx "Integer behavior given more than once"; Exception))
  ] args

let parse_string_args ctx args =
  parse_all_arguments ctx { null_as_empty_string = false; undefined_as=Undefined } [
    ArgEqualsSpecific("TreatNullAs", [("EmptyString",
                                      fun state ->
                                        if state.null_as_empty_string then
                                          warn ctx "TreatNullAs given twice";
                                        { state with null_as_empty_string = true })]);
    ArgEqualsSpecific("TreadUndefinedAs", [
      ("EmptyString", fun state ->
         if state.undefined_as <> Undefined then warn ctx "TreatUndefinedAs given twice";
         { state with undefined_as = EmptyString });
      ("Null", fun state ->
         if state.undefined_as <> Undefined then warn ctx "TreatUndefinedAs given twice";
         { state with undefined_as = Null })
    ])
  ] args

let translate_base_type ctx args = function
  | Ast.ShortType ->
      IntType { length = Short; unsigned = false; out_of_range = parse_int_args ctx args }
  | Ast.LongType ->
      IntType { length = Long; unsigned = false; out_of_range = parse_int_args ctx args }
  | Ast.LongLongType ->
      IntType { length = LongLong; unsigned = false; out_of_range = parse_int_args ctx args }
  | Ast.UnsignedShortType ->
      IntType { length = Short; unsigned = true; out_of_range = parse_int_args ctx args }
  | Ast.UnsignedLongType ->
      IntType { length = Long; unsigned = true; out_of_range = parse_int_args ctx args }
  | Ast.UnsignedLongLongType ->
      IntType { length = LongLong; unsigned = true; out_of_range = parse_int_args ctx args }
  | Ast.FloatType -> FloatType { double = false; unrestricted = false }
  | Ast.DoubleType -> FloatType { double = true; unrestricted = false }
  | Ast.UnrestrictedFloatType -> FloatType { double = false; unrestricted = true }
  | Ast.UnrestrictedDoubleType -> FloatType { double = true; unrestricted = true }
  | Ast.DOMStringType -> DOMStringType (parse_string_args ctx args)
  | Ast.NamedType n -> NamedType n
  | Ast.AnyType -> AnyType
  | Ast.VoidType -> VoidType
  | Ast.OctetType -> OctetType
  | Ast.ByteType -> ByteType
  | Ast.BooleanType -> BooleanType
  | Ast.DateType -> DateType
  | Ast.ObjectType -> ObjectType

let rec translate_type ctx args = function
  | Ast.TypeLeaf b -> translate_base_type ctx args b
  | Ast.TypeUnion u -> UnionType (List.map (translate_type ctx args) u)
  | Ast.TypeArray t -> ArrayType (translate_type ctx args t)
  | Ast.TypeOption t -> 
      let (treat_undefined_as_missing, args) =
        parse_some_arguments ctx false [
          ArgEqualsSpecific("TreatUndefinedAs", [("Missing", fun _ -> true)])] args in
      OptionType (treat_undefined_as_missing, translate_type ctx args t)
  | Ast.TypeNullable t -> NullableType (translate_type ctx args t)
  | Ast.TypeSequence t -> SequenceType (translate_type ctx args t)
let translate_type ctx ty args = translate_type ctx args ty

let translate_value ctx (x: Ast.value): value = x

let translate_constant ctx (name, types, value) = 
  { name; types = translate_type ctx types []; value = translate_value ctx value }

let translate_attribute ctx (name, inherited, read_only, types) attrs =
  let (lenient_this, attrs) =
    parse_some_arguments ctx false [
      ArgPlain ("LenientThis",
                function true -> warn ctx "LenientThis given twice"; true | false -> true)]
      attrs
  in let (access, attrs) =
    parse_some_arguments ctx (if read_only then ReadOnly else ReadWrite) [
      ArgEquals ("PutForwards", fun access name ->
                   if access <> ReadOnly then warn ctx "Inconsistent access mode";
                   PutForwards name);
      ArgPlain ("Replacable", fun access ->
                  if access <> ReadOnly then warn ctx "Inconsistent access mode";
                  Replacable);
      ArgPlain ("Unforgable", fun access ->
                  if access <> ReadOnly then warn ctx "Inconsistent access mode";
                  Unforgable)
    ] attrs
  in { name; types = translate_type ctx types attrs; lenient_this; inherited; access }

let translate_argument ctx (((name, types, mode, default): Ast.argument_data), attrs) =
  let kind = match mode with
    | Ast.ModeSingle ->
        if default <> None then warn ctx "Default given for non-optional argument";
        Single
    | Ast.ModeMultiple ->
        if default <> None then warn ctx "Default given for non-optional argument";
        Multiple
    | Ast.ModeOptional ->
        match default with
          | None -> Optional
          | Some default -> Default default
  in { name; types = translate_type ctx types attrs ; kind }
let translate_arguments ctx args = List.map (translate_argument ctx) args

let translate_return_type ctx return attrs =
  translate_type ctx return
    (limit_arguments ctx ["TreatNullAs"; "TreadUndefinedAs"] attrs)

let translate_regular_operation ctx (name: string option) return args attrs: operation = 
  match name with Some name -> 
    { name;
      return = translate_return_type ctx return attrs;
      args = translate_arguments ctx args }
    | None -> fail ctx "Unnamed regular operation"

let translate_legacy_caller ctx name return args attrs =
  ( { return = translate_return_type ctx return attrs;
      args = translate_arguments ctx args },
    match name with
      | Some _ -> [ translate_regular_operation ctx name return args attrs ]
      | None  -> [])

let translate_special
      ctx qualifier name return (args: Ast.argument_list)
      attrs indexed_properties named_properties =
  ((match qualifier, List.map (fun ((_, ty, _, _), attrs) -> (ty, attrs)) args with
    | Ast.SpecGetter, [ (Ast.TypeLeaf Ast.UnsignedLongType, argattrs) ] ->
        if argattrs <> [] then warn ctx "Attributes given for index argument";
        ({ indexed_properties with getter = Some (translate_return_type ctx return attrs) },
         named_properties)
    | Ast.SpecGetter, [ (Ast.TypeLeaf Ast.DOMStringType, argattrs) ] ->
        if argattrs <> [] then warn ctx "Attributes given for index argument";
        (indexed_properties,
         { named_properties with getter = Some (translate_return_type ctx return attrs) })
    | Ast.SpecDeleter, [ (Ast.TypeLeaf Ast.UnsignedLongType, argattrs) ] ->
        if argattrs <> [] then warn ctx "Attributes given for index argument";
        ({ indexed_properties with deleter = Some (translate_return_type ctx return attrs) },
         named_properties)
    | Ast.SpecDeleter, [ (Ast.TypeLeaf Ast.DOMStringType, argattrs) ] ->
        if argattrs <> [] then warn ctx "Attributes given for index argument";
        (indexed_properties,
         { named_properties with deleter = Some (translate_return_type ctx return attrs) })
    | Ast.SpecSetter, [ (Ast.TypeLeaf Ast.UnsignedLongType, argattrs);
                        (arg, argattrs') ] ->
        if argattrs <> [] then warn ctx "Attributes given for index argument";
        ({ indexed_properties with setter =
             Some (translate_return_type ctx return attrs, translate_type ctx arg argattrs') },
         named_properties)
    | Ast.SpecSetter, [ (Ast.TypeLeaf Ast.DOMStringType, argattrs);
                        (arg, argattrs') ] ->
        if argattrs <> [] then warn ctx "Attributes given for index argument";
        (indexed_properties,
         { named_properties with setter =
             Some (translate_return_type ctx return attrs, translate_type ctx arg argattrs') })
    | Ast.SpecCreator, [ (Ast.TypeLeaf Ast.UnsignedLongType, argattrs);
                         (arg, argattrs') ] ->
        if argattrs <> [] then warn ctx "Attributes given for index argument";
        ({ indexed_properties with creator =
             Some (translate_return_type ctx return attrs, translate_type ctx arg argattrs') },
         named_properties)
    | Ast.SpecCreator, [ (Ast.TypeLeaf Ast.DOMStringType, argattrs);
                         (arg, argattrs') ] ->
        if argattrs <> [] then warn ctx "Attributes given for index argument";
        (indexed_properties,
         { named_properties with creator =
             Some (translate_return_type ctx return attrs, translate_type ctx arg argattrs') })
    | _, _ -> fail ctx "Unparsable special operation"),
   match name with
     | Some _ ->
         let args = List.map (fun ((name, ty, _, _), attrs) ->
                                ((name, ty, Ast.ModeSingle, None), attrs)) args in
         [ translate_regular_operation ctx name return args attrs ]
     | None -> [])

let translate_operation ctx name return arguments qualifiers attrs interface =
  let name = match name with "" -> None | _ -> Some name in
  match qualifiers with
    | [] ->
        let op = translate_regular_operation ctx name return arguments attrs in
          { interface with operations = op :: interface.operations }
    | [Ast.SpecLegacyCaller] ->
        let (legacy, op) = translate_legacy_caller ctx name return arguments attrs in
          { interface with operations = op @ interface.operations;
                           legacy_callers = legacy :: interface.legacy_callers }
    | [Ast.SpecStatic] ->
        let op = translate_regular_operation ctx name return arguments attrs in
          { interface with static_operations = op :: interface.static_operations }
    | [qual] ->
        let ((indexed_properties, named_properties), op) =
          translate_special ctx qual name return arguments attrs
            interface.indexed_properties interface.named_properties
        in
          { interface with indexed_properties; named_properties;
                           operations = op @ interface.operations }
    | _ ->
        fail ctx "Invalid qualifer combination given"

let translate_stringifer_empty ctx attrs = InternalStringifer (parse_string_args ctx attrs)
let translate_stringifier_operation ctx name return args attrs =
  begin if return <> Ast.TypeLeaf Ast.DOMStringType then
    warn ctx "Stringifier does not return string"
  end;
  begin if args <> [] then
    warn ctx "Stringifier takes arguments"
  end;
  ( InternalStringifer (parse_string_args ctx attrs),
    match name with
      | Some _ -> [ translate_regular_operation ctx name return args attrs ]
      | None -> [])
let translate_stringifier_attribute ctx name inherited readonly types attrs =
  ( AttributeStringifier (name, parse_string_args ctx attrs),
    translate_attribute ctx (name, inherited, readonly, types) attrs)

let translate_member ctx interface (member, attrs) =
  let update_stringifier stringifier =
    if interface.stringifier <> NoStringifier then warn ctx "More than one stringifier";
    { interface with stringifier }
  in match member with
    | Ast.StringifierEmptyMember -> 
        { interface with stringifier = translate_stringifer_empty ctx attrs }
    | Ast.StringifierOperationMember (name, return, args) ->
        let (str, op) = translate_stringifier_operation ctx (Some name) return args attrs in
        let interface' = update_stringifier str in
          { interface' with operations = op @ interface.operations }
    | Ast.StringifierAttributeMember (name, inherited, readonly, types) ->
        let (str, attr) =
          translate_stringifier_attribute ctx name inherited readonly types attrs in
        let interface' = update_stringifier str in
          { interface' with attributes = attr :: interface.attributes }
    | Ast.OperationMember  (name, return, arguments, qualifiers) ->
        translate_operation ctx name return arguments qualifiers attrs interface
    | Ast.AttributeMember (name, inherited, readonly, types) ->
        let attr = translate_attribute ctx (name, inherited, readonly, types) attrs in
          { interface with attributes = attr :: interface.attributes }
    | Ast.ConstMember (name, types, value) ->
        begin if attrs = [] then warn ctx "Constant with attribute" end;
        let value = translate_constant ctx (name, types, value) in
          { interface with consts = value :: interface.consts }

let parse_constructor ctx name constructors (args: Ast.argument_list option) =
  { name; args = translate_arguments ctx (BatOption.default [] args)} :: constructors

let translate_interface ctx (name, mode, members) attrs =
  let (inheritance_mode, attrs) =
    let (is_array, attrs) = parse_some_arguments ctx false
                              [ArgPlain("ArrayClass", fun _ -> true)] attrs
    in match mode with
    | Ast.ModePartial ->
        fail ctx "Partial interfaces should have been resolved at this point!"
    | Ast.ModeInherit from ->
        if is_array then warn ctx "Trying to perform double inheritance";
        (InheritsFrom from, attrs)
    | Ast.ModeTop ->
        ((if is_array then ArrayClass else Toplevel), attrs)
  and empty_properties = { getter = None; setter = None; deleter = None; creator = None }
  in let (constructors, attrs) =
    parse_some_arguments ctx [] [
      ArgMaybeArguments ("Constructor", (parse_constructor ctx name));
      ArgMaybeArgumentsEquals ("NamedConstructor", (fun a b -> parse_constructor ctx b a))
    ] attrs
  in let ((special, not_exposed), attrs) =
    parse_some_arguments ctx
      ({ this_lenient = false; this_implicit = false;
         named_properties_object = false;
         override_builtins = false }, false)
      [
        ArgPlain ("LenientThis",
                  (fun (special, not_exposed) -> 
                     ({ special with this_lenient = true }, not_exposed)));
        ArgPlain ("ImplicitThis",
                  (fun (special, not_exposed) -> 
                     ({ special with this_implicit = true }, not_exposed)));
        ArgPlain ("OverrideBuiltins",
                  (fun (special, not_exposed) -> 
                     ({ special with override_builtins = true },
                      not_exposed)));
        ArgPlain ("NamedPropertiesObject",
                  (fun (special, not_exposed) -> 
                     ({ special with named_properties_object = true },
                      not_exposed)));
        ArgPlain ("NoInterfaceObject",
                  (fun (special, _) -> (special, true)))
      ] attrs
  in let init = {
    inheritance_mode; name; special; not_exposed; constructors;
    consts = []; attributes = []; operations = [];
    static_operations = [];
    named_properties = empty_properties;
    indexed_properties = empty_properties;
    legacy_callers = [];
    stringifier = NoStringifier
  }
  in List.fold_left (translate_member ctx) init members


let translate_dictionary_entry ctx ((name, types, default_value), attrs) =
  { name; default_value = BatOption.map (translate_value ctx) default_value;
    types = translate_type ctx types (limit_arguments ctx ["Clamp"; "EnforceRange"] attrs) }
let translate_dictionary ctx (name, mode, members) =
  let inherits_from = match mode with
    | Ast.ModeTop -> None
    | Ast.ModeInherit from -> Some from
    | Ast.ModePartial -> fail ctx "At this point, no partial dictionaries should remain"
  in { name; inherits_from; members = List.map (translate_dictionary_entry ctx) members }

let translate_exception_member ctx name types attrs =
  { name; types = translate_type ctx types (limit_arguments ctx [] attrs) }
let translate_exception_member' ctx exc (member, attrs) = match member with
  | Ast.ExConstMember const ->
      if attrs <> [] then warn ctx "Attributes given on constant";
      { exc with consts = translate_constant ctx const :: exc.consts }
  | Ast.ExValueMember (name, types) ->
      { exc with
            members = translate_exception_member ctx name types attrs :: exc.members }
let translate_exception ctx (name, inherits_from, members) attrs =
  let not_exposed =
    parse_all_arguments ctx false [ArgPlain ("NoInterfaceObject", fun _ -> true)] attrs
  in List.fold_left (translate_exception_member' ctx)
       { name; inherits_from; not_exposed; consts = []; members = [] } members

let translate_enumeration ctx (name, values) attrs =
  if attrs <> [] then warn ctx "Attributes given on enumeration";
  { name; values }

let translate_callback ctx (name, return, args) attrs =
  let treat_non_callable_as_null = parse_all_arguments ctx false
                                     [ArgPlain ("TreatNonCallableAsNull", fun _ -> true)]
                                     attrs
  in let ({ name; return; args }: operation) =
    translate_regular_operation ctx name return args []
  in { name; return; args; treat_non_callable_as_null }

let translate_callback_interface ctx desc attrs =
  translate_interface ctx desc
    (forbid_arguments ctx ["Constructor"; "NamedConstructor"; "NoInterfaceObject"] attrs)


let translate_definitions =
    List.fold_left
      (fun defs (def, attrs) -> match def with
         | Ast.DefEnum e ->
             let e = translate_enumeration () e attrs in
               { defs with enumerations = StringMap.add e.name e defs.enumerations }
         | Ast.DefException e ->
             let e = translate_exception () e attrs in
               { defs with exceptions = StringMap.add e.name e defs.exceptions }
         | Ast.DefCallback (name, ret, args) ->
             let c = translate_callback () (Some name, ret, args) attrs in
               { defs with callbacks = StringMap.add c.name c defs.callbacks }
         | Ast.DefInterface i ->
             let i = translate_interface () i attrs in
               { defs with interfaces = StringMap.add i.name i defs.interfaces }
         | Ast.DefCallbackInterface i ->
             let i = translate_interface () i attrs in
               { defs with callback_interfaces =
                   StringMap.add i.name i defs.callback_interfaces }
         | Ast.DefDictionary d ->
             if attrs <> [] then warn () "Attributes given for dictionary";
             let d = translate_dictionary () d in
               { defs with dictionaries = StringMap.add d.name d defs.dictionaries }
         | Ast.DefImplements (lower, upper) ->
             { defs with implements = (lower, upper) :: defs.implements }
         | Ast.DefTypedef _ ->
             fail () "Typedef in definitions; please clean up first"
      ) { dictionaries = StringMap.empty;
          enumerations = StringMap.empty;
          interfaces = StringMap.empty;
          exceptions = StringMap.empty;
          callbacks = StringMap.empty;
          callback_interfaces = StringMap.empty;
          implements = [] }
