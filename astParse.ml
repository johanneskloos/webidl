open IdlData
open ContextError
open AstParseExtendedAttributes
open SimpleAst
(** Second step: Translate the AST to a nicer form *)

let update_if_default what defvalue newvalue ctx curvalue =
  if curvalue <> defvalue then begin
    warn ctx "%s given more than once" what
  end;
  newvalue

let parse_int_args ctx args =
  handle_all_known Modulo ctx [
    xattr_plain "Clamp" (update_if_default "Integer behavior" Modulo Clamp);
    xattr_plain "EnforceRange"  (update_if_default "Integer behavior" Modulo Exception)
  ] args

let parse_string_args ctx args =
  handle_all_known
    { null_as_empty_string = false; undefined_as=Undefined }
    ctx
    [
      xattr_equals_specific "TreatNullAs" [
        ("EmptyString", fun ({ null_as_empty_string } as state) ->
           { state with null_as_empty_string =
               update_if_default "Null string behavior" false true
                 ctx null_as_empty_string })
      ];
      xattr_equals_specific "TreadUndefinedAs" [
        ("EmptyString", fun ({ undefined_as } as state) ->
           { state with undefined_as =
               update_if_default "Undefined string behavior" Undefined EmptyString
                 ctx undefined_as });
        ("NullString", fun ({ undefined_as } as state) ->
           { state with undefined_as =
               update_if_default "Undefined string behavior" Undefined Null
                 ctx undefined_as })
      ]
    ] args

    (*
let translate_base_type ctx args = function
  | SimpleAst.ShortType ->
      let (out_of_range, user_args) = parse_int_args ctx args in
      (IntType { length = Short; unsigned = false; out_of_range }, user_args)
  | SimpleAst.LongType ->
      let (out_of_range, user_args) = parse_int_args ctx args in
      (IntType { length = Long; unsigned = false; out_of_range }, user_args)
  | SimpleAst.LongLongType ->
      let (out_of_range, user_args) = parse_int_args ctx args in
      (IntType { length = LongLong; unsigned = false; out_of_range }, user_args)
  | SimpleAst.UnsignedShortType ->
      let (out_of_range, user_args) = parse_int_args ctx args in
      (IntType { length = Short; unsigned = true; out_of_range }, user_args)
  | SimpleAst.UnsignedLongType ->
      let (out_of_range, user_args) = parse_int_args ctx args in
      (IntType { length = Long; unsigned = true; out_of_range }, user_args)
  | SimpleAst.UnsignedLongLongType ->
      let (out_of_range, user_args) = parse_int_args ctx args in
      (IntType { length = LongLong; unsigned = true; out_of_range }, user_args)
  | SimpleAst.FloatType -> (FloatType { double = false; unrestricted = false }, args)
  | SimpleAst.DoubleType -> (FloatType { double = true; unrestricted = false }, args)
  | SimpleAst.UnrestrictedFloatType -> (FloatType { double = false; unrestricted = true }, args)
  | SimpleAst.UnrestrictedDoubleType -> (FloatType { double = true; unrestricted = true }, args)
  | SimpleAst.DOMStringType ->
      let (string_args, user_args) = parse_string_args ctx args in
      (DOMStringType string_args, user_args)
  | SimpleAst.NamedType n -> (NamedType n, args)
  | SimpleAst.AnyType -> (AnyType, args)
  | SimpleAst.VoidType -> (VoidType, args)
  | SimpleAst.OctetType -> (OctetType, args)
  | SimpleAst.ByteType -> (ByteType, args)
  | SimpleAst.BooleanType -> (BooleanType, args)
  | SimpleAst.DateType -> (DateType, args)
  | SimpleAst.ObjectType -> (ObjectType, args)

let intersect l1 l2 = List.filter (fun x -> List.mem x l1) l2

let rec translate_type ctx args = function
  | SimpleAst.TypeLeaf b -> translate_base_type ctx args b
  | SimpleAst.TypeUnion u ->
      let (types, user_attrs) = List.split (List.map (translate_type ctx args) u)
      in ((UnionType types), List.fold_left intersect args user_attrs)
  | SimpleAst.TypeArray t ->
      let (type_, user_attrs) = translate_type ctx args t in (ArrayType type_, user_attrs)
  | SimpleAst.TypeOption t -> 
      let (treat_undefined_as_missing, remaining_attrs) =
        handle_non_failing_known false ctx [
          xattr_equals_specific "TreatUndefinedAs" [
            "Missing", update_if_default "undefined handling" false true ctx
          ]
        ] args 
      in let (type_, user_attrs) = translate_type ctx remaining_attrs t
      in (OptionType(treat_undefined_as_missing, type_), user_attrs)
  | SimpleAst.TypeNullable t ->
      let (type_, user_attrs) = translate_type ctx args t in (NullableType type_, user_attrs)
  | SimpleAst.TypeSequence t ->
      let (type_, user_attrs) = translate_type ctx args t in (SequenceType type_, user_attrs)
let translate_type ctx ty args = translate_type ctx args ty

let translate_value ctx (x: SimpleAst.value): value = x

type ('a, 'b) either = Left of 'a | Right of 'b
let rec partition_map f = function
  | [] -> ([], [])
  | x::l -> let (ll, lr) = partition_map f l in match f x with
      | Left y -> (y::ll, lr)
      | Right y -> (ll, y::lr)

let translate_mode_and_default ctx mode default = match mode with
  | SimpleAst.ModeSingle ->
      if default <> None then warn ctx "Default given for single argument";
      Single
  | SimpleAst.ModeMultiple ->
      if default <> None then warn ctx "Default given for variadic argument";
      Multiple
  | SimpleAst.ModeOptional ->
      match default with
        | Some default -> Default default
        | None -> Optional

let rec translate_attributes ctx attrs =
  List.map (function
              | SimpleAst.WithoutArguments(name, None) -> UAPlain name
              | SimpleAst.WithoutArguments(name, Some id) -> UAEquals (name, id)
              | SimpleAst.WithArguments(name, None, args) ->
                  UAArguments (name, translate_arguments ctx args)
              | SimpleAst.WithArguments(name, Some id, args) ->
                  UAArgumentsEquals (name, id, translate_arguments ctx args))
    attrs
and translate_argument ctx (((name, types, mode, default): SimpleAst.argument_data), attrs) =
  let ctx = ctx_push_scope ctx "Argument %s" name in
  let kind = translate_mode_and_default ctx mode default in
  let (types, user_attributes) = translate_type ctx types attrs in
    { name; kind; user_attributes = translate_attributes ctx user_attributes; types }
and translate_arguments ctx args = List.map (translate_argument ctx) args


let translate_constant ctx (name, types, value) attrs = 
  let (types, user_attributes) = translate_type ctx types attrs in
  let ctx = ctx_push_scope ctx "Constant %s" name in
  { name; types; user_attributes = translate_attributes ctx user_attributes;
    value = translate_value ctx value }

let translate_attribute ctx (name, inherited, read_only, types) attrs =
  let ctx = ctx_push_scope ctx "Attribute %s" name
  and update_access = update_if_default "Access mode" ReadOnly in
  let ((lenient_this, access), attrs) =
    handle_all_known (false, if read_only then ReadOnly else ReadWrite) ctx [
      xattr_plain "LenientThis" (fun ctx (lenient_this, access) ->
                                   (update_if_default "LenientThis" false true ctx
                                      lenient_this,
                                    access));
      xattr_equals "PutForwards" (fun ctx (lenient_this, access) fwto ->
                                    (lenient_this,
                                     update_access (PutForwards fwto) ctx access));
      xattr_plain "Replacable" (fun ctx (lenient_this, access) ->
                                  (lenient_this, update_access Replacable ctx access));
      xattr_plain "Unforgable" (fun ctx (lenient_this, access) ->
                                  (lenient_this, update_access Unforgable ctx access))
    ] attrs in
  let (types, attrs) = translate_type ctx types attrs in
  { name; lenient_this; inherited; access; types;
    user_attributes = translate_attributes ctx attrs }

let translate_return_type ctx return attrs =
  let ctx = ctx_push_scope ctx "Return type" in
    translate_type ctx return
      (drop_bad_attributes ctx ["TreatNullAs"; "TreatUndefinedAs"] attrs)

let translate_regular_operation ctx (name: string option) return args attrs: operation = 
  let name = match name with
    | Some name -> name 
    | None -> error ctx "Unnamed regular operation"; "???"
  in
  let ctx = ctx_push_scope ctx "Operation %s" name in
  let (return, attrs) = translate_return_type ctx return attrs in
    { name; return; user_attributes = translate_attributes ctx attrs;
      args = translate_arguments ctx args
    }

let without_name = Fmt.const Fmt.string " (without name)"

let translate_legacy_caller ctx name return' args attrs =
  let ctx = ctx_push_scope ctx "Legacy operation %a" (Fmt.option ~none:without_name Fmt.string) name in
  let (return, attrs) = translate_return_type ctx return' attrs in
  ( { return;
      args = translate_arguments ctx args;
      user_attributes = translate_attributes ctx attrs
  },
    match name with
      | Some _ -> [ translate_regular_operation ctx name return' args attrs ]
      | None  -> [])

let translate_special
      ctx qualifier name return (args: SimpleAst.argument_list)
      attrs indexed_properties named_properties =
  let ctx = ctx_push_scope ctx "Special operation %a" (Fmt.option ~none:without_name Fmt.string) name in
  let translate_attributed_type translate ctx ty attrs =
    let (types, attrs) = translate ctx ty attrs in
      { types; user_attributes = translate_attributes ctx attrs }
  in let translate_one return attrs = 
    Some (translate_attributed_type translate_return_type ctx return attrs)
  and translate_two arg argattrs return retattrs =
    Some (translate_attributed_type translate_return_type ctx return retattrs,
          translate_attributed_type translate_type ctx arg argattrs)
  in
  ((match qualifier, List.map (fun ((_, ty, _, _), attrs) -> (ty, attrs)) args with
    | SimpleAst.SpecGetter, [ (SimpleAst.TypeLeaf SimpleAst.UnsignedLongType, argattrs) ] ->
        if argattrs <> [] then warn ctx "Attributes given for index argument";
        ({ indexed_properties with getter = translate_one return attrs },
         named_properties)
    | SimpleAst.SpecGetter, [ (SimpleAst.TypeLeaf SimpleAst.DOMStringType, argattrs) ] ->
        if argattrs <> [] then warn ctx "Attributes given for index argument";
        (indexed_properties,
         { named_properties with getter = translate_one return attrs })
    | SimpleAst.SpecDeleter, [ (SimpleAst.TypeLeaf SimpleAst.UnsignedLongType, argattrs) ] ->
        if argattrs <> [] then warn ctx "Attributes given for index argument";
        ({ indexed_properties with deleter = translate_one return attrs },
         named_properties)
    | SimpleAst.SpecDeleter, [ (SimpleAst.TypeLeaf SimpleAst.DOMStringType, argattrs) ] ->
        if argattrs <> [] then warn ctx "Attributes given for index argument";
        (indexed_properties,
         { named_properties with deleter = translate_one return attrs })
    | SimpleAst.SpecSetter, [ (SimpleAst.TypeLeaf SimpleAst.UnsignedLongType, retattrs);
                        (arg, argattrs) ] ->
        ({ indexed_properties with setter = translate_two arg argattrs return retattrs },
         named_properties)
    | SimpleAst.SpecSetter, [ (SimpleAst.TypeLeaf SimpleAst.DOMStringType, retattrs);
                        (arg, argattrs) ] ->
        (indexed_properties,
         { named_properties with setter = translate_two arg argattrs return retattrs })
    | SimpleAst.SpecCreator, [ (SimpleAst.TypeLeaf SimpleAst.UnsignedLongType, retattrs);
                         (arg, argattrs) ] ->
        ({ indexed_properties with creator = translate_two arg argattrs return retattrs },
         named_properties)
    | SimpleAst.SpecCreator, [ (SimpleAst.TypeLeaf SimpleAst.DOMStringType, retattrs);
                         (arg, argattrs) ] ->
        (indexed_properties,
         { named_properties with creator = translate_two arg argattrs return retattrs })
    | _, _ -> error ctx "Unparsable special operation";
              (indexed_properties, named_properties)),
   match name with
     | Some _ ->
         let args = List.map (fun ((name, ty, _, _), attrs) ->
                                ((name, ty, SimpleAst.ModeSingle, None), attrs)) args in
         [ translate_regular_operation ctx name return args attrs ]
     | None -> [])

let translate_operation ctx name return arguments qualifiers attrs interface =
  let name = match name with "" -> None | _ -> Some name in
  match qualifiers with
    | [] ->
        let op = translate_regular_operation ctx name return arguments attrs in
          { interface with operations = op :: interface.operations }
    | [SimpleAst.SpecLegacyCaller] ->
        let (legacy, op) = translate_legacy_caller ctx name return arguments attrs in
          { interface with operations = op @ interface.operations;
                           legacy_callers = legacy :: interface.legacy_callers }
    | [SimpleAst.SpecStatic] ->
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
        error ctx "Invalid qualifer combination given";
        interface

let translate_stringifer_empty ctx attrs =
  let ctx = ctx_push_scope ctx "Anonymous stringifier" in
  let (string_mode, attrs) = parse_string_args ctx attrs in
  InternalStringifer (string_mode, translate_attributes ctx attrs)

let translate_stringifier_operation ctx name return args attrs =
  let ctx = ctx_push_scope ctx "Stringifier operation %a" (Fmt.option ~none:without_name Fmt.string) name in
  begin if return <> SimpleAst.TypeLeaf SimpleAst.DOMStringType then
    warn ctx "Stringifier does not return string"
  end;
  begin if args <> [] then
    warn ctx "Stringifier takes arguments"
  end;
  let (string_mode, attrs) = parse_string_args ctx attrs in
  ( InternalStringifer (string_mode, translate_attributes ctx attrs),
    match name with
      | Some _ -> [ translate_regular_operation ctx name return args attrs ]
      | None -> [])

let translate_stringifier_attribute ctx name inherited readonly types attrs =
  let ctx = ctx_push_scope ctx "Stringifier attribute %s" name in
  let (string_mode, attrs) = parse_string_args ctx attrs in
  ( AttributeStringifier (name, string_mode, translate_attributes ctx attrs),
    translate_attribute ctx (name, inherited, readonly, types) attrs)

let translate_member ctx interface (member, attrs) =
  let update_stringifier stringifier =
    if interface.stringifier <> NoStringifier then warn ctx "More than one stringifier";
    { interface with stringifier }
  in match member with
    | SimpleAst.StringifierEmptyMember -> 
        { interface with stringifier = translate_stringifer_empty ctx attrs }
    | SimpleAst.StringifierOperationMember (name, return, args) ->
        let (str, op) = translate_stringifier_operation ctx (Some name) return args attrs in
        let interface' = update_stringifier str in
          { interface' with operations = op @ interface.operations }
    | SimpleAst.StringifierAttributeMember (name, inherited, readonly, types) ->
        let (str, attr) =
          translate_stringifier_attribute ctx name inherited readonly types attrs in
        let interface' = update_stringifier str in
          { interface' with attributes = attr :: interface.attributes }
    | SimpleAst.OperationMember  (name, return, arguments, qualifiers) ->
        translate_operation ctx name return arguments qualifiers attrs interface
    | SimpleAst.AttributeMember (name, inherited, readonly, types) ->
        let attr = translate_attribute ctx (name, inherited, readonly, types) attrs in
          { interface with attributes = attr :: interface.attributes }
    | SimpleAst.ConstMember (name, types, value) ->
        let value = translate_constant ctx (name, types, value) attrs in
          { interface with consts = value :: interface.consts }

let parse_constructor ctx name constructors (args: SimpleAst.argument_list option) =
  let ctx = ctx_push_scope ctx "Constructor %a" pp_qualified_name name in
  { name; user_attributes = [];
    args = translate_arguments ctx (BatOption.default [] args)
  } :: constructors

type interface_info = {
  is_array: bool;
  constructors: constructor list;
  this_implicit: bool;
  this_lenient: bool;
  override_builtins: bool;
  named_properties_object: bool;
  not_exposed: bool
}

let translate_interface ctx (name, mode, members) attrs =
  let ctx = ctx_push_scope ctx "Interface %a" pp_qualified_name name in
  let ({ is_array; constructors; this_implicit; this_lenient;
         override_builtins; named_properties_object; not_exposed }, attrs) =
    handle_all_known
      { is_array = false; constructors = []; not_exposed = false;
        this_lenient = false; this_implicit = false;
        named_properties_object = false; override_builtins = false }
      ctx
      [
        xattr_plain "ArrayClass"
          (fun ctx ({ is_array } as state) ->
             { state with is_array =
                 update_if_default "ArrayClass" false true ctx is_array });
        xattr_plain "LenientThis"
          (fun ctx ({ this_lenient } as state) ->
             { state with this_lenient =
                 update_if_default "LenientThis" false true ctx this_lenient });
        xattr_plain "ImplicitThis"
          (fun ctx ({ this_implicit } as state) ->
             { state with this_implicit =
                 update_if_default "ImplicitThis" false true ctx this_implicit });
        xattr_plain "OverrideBuiltins"
          (fun ctx ({ override_builtins } as state) ->
             { state with override_builtins =
                 update_if_default "OverrideBuiltins" false true ctx override_builtins });
        xattr_plain "OverrideBuiltins"
          (fun ctx ({ named_properties_object } as state) ->
             { state with named_properties_object =
                 update_if_default "OverrideBuiltins" false true ctx
                   named_properties_object });
        xattr_maybe_arguments "Constructor"
          (fun ctx ({ constructors } as state) args ->
             { state with constructors =
                 parse_constructor ctx name constructors args });
        xattr_equals_maybe_arguments "NamedConstructor"
          (fun ctx ({ constructors } as state) name' args ->
             { state with constructors =
                 (* TODO check if we need to module-qualify this earlier. *)
                 let name = BatString.nsplit name' ~by:"::" in
                 parse_constructor ctx name constructors args });
        xattr_plain "NoInterfaceObject"
          (fun ctx ({ not_exposed } as state) ->
             { state with not_exposed =
                 update_if_default "NoInterfaceObject" false true ctx not_exposed })
      ]
      attrs
  in let inheritance_mode = match mode with
    | SimpleAst.ModePartial ->
        error ctx "Partial interfaces should have been resolved at this point!";
        Toplevel
    | SimpleAst.ModeInherit from ->
        if is_array then warn ctx "Trying to perform double inheritance";
        InheritsFrom from
    | SimpleAst.ModeTop ->
        if is_array then ArrayClass else Toplevel
  and empty_properties = { getter = None; setter = None; deleter = None; creator = None }
  in let init = {
    inheritance_mode; name; not_exposed; constructors;
    user_attributes = translate_attributes ctx attrs;
    special = { this_lenient; this_implicit; named_properties_object; override_builtins };
    consts = []; attributes = []; operations = [];
    static_operations = [];
    named_properties = empty_properties;
    indexed_properties = empty_properties;
    legacy_callers = [];
    stringifier = NoStringifier
  }
  in List.fold_left (translate_member ctx) init members


let translate_dictionary_entry ctx ((name, types, default_value), attrs) =
  let ctx = ctx_push_scope ctx "Dictionary entry %s" name in
  let (types, attrs) =
    translate_type ctx types (keep_good_attributes ["Clamp"; "EnforceRange"] attrs) in
  { name; types;
    user_attributes = translate_attributes ctx attrs;
    default_value = BatOption.map (translate_value ctx) default_value
  }

let translate_dictionary ctx (name, mode, members) attrs =
  let ctx = ctx_push_scope ctx "Dictionary %a" pp_qualified_name name in
  let inherits_from = match mode with
    | SimpleAst.ModeTop -> None
    | SimpleAst.ModeInherit from -> Some from
    | SimpleAst.ModePartial ->
        error ctx "At this point, no partial dictionaries should remain"; None
  in { name; inherits_from; user_attributes = translate_attributes ctx attrs;
       members = List.map (translate_dictionary_entry ctx) members
  }

let translate_exception_member ctx name types attrs =
  let ctx = ctx_push_scope ctx "Exception member %s" name in
  let (types, attrs) = translate_type ctx types attrs in
  { name; types; user_attributes = translate_attributes ctx attrs }

let translate_exception_member' ctx exc (member, attrs) = match member with
  | SimpleAst.ExConstMember const ->
      { exc with consts = translate_constant ctx const attrs :: exc.consts }
  | SimpleAst.ExValueMember (name, types) ->
      { exc with
            members = translate_exception_member ctx name types attrs :: exc.members }
let translate_exception ctx (name, inherits_from, members) attrs =
  let ctx = ctx_push_scope ctx "Exception %a" pp_qualified_name name in
  let (not_exposed, attrs) =
    handle_all_known false ctx [
      xattr_plain "NoInterfaceObject" (update_if_default "NoInterfaceObject" false true)
    ] attrs
  in List.fold_left (translate_exception_member' ctx)
       { name; inherits_from; not_exposed;
         user_attributes = translate_attributes ctx attrs;
         consts = [];
         members = [];
       } members

let translate_enumeration ctx (name, values) attrs =
  let ctx = ctx_push_scope ctx "Enumeration %a" pp_qualified_name name in
  { name; values; user_attributes = translate_attributes ctx attrs }

let translate_callback ctx (name, return, args) attrs =
  let ctx = ctx_push_scope ctx "Callback %a" pp_qualified_name name in
  let (treat_non_callable_as_null, attrs) =
    handle_all_known false ctx [
      xattr_plain "TreatNonCallableAsNull"
        (update_if_default "TreatNonCallableAsNull" false true)
    ] attrs
  in let (return, _) = translate_return_type ctx return []
  and args = translate_arguments ctx args
  in { name; return; args; treat_non_callable_as_null;
       user_attributes = translate_attributes ctx attrs }

let translate_callback_interface ctx desc attrs =
  translate_interface ctx desc
    (drop_bad_attributes ctx
       ["Constructor"; "NamedConstructor"; "NoInterfaceObject"] attrs)


let translate_definitions =
  let top = ctx_top () in
    List.fold_left
      (fun defs (def, attrs) -> match def with
         | SimpleAst.DefEnum e ->
             let e = translate_enumeration top e attrs in
               { defs with enumerations = QNameMap.add e.name e defs.enumerations }
         | SimpleAst.DefException e ->
             let e = translate_exception top e attrs in
               { defs with exceptions = QNameMap.add e.name e defs.exceptions }
         | SimpleAst.DefCallback (name, ret, args) ->
             let c = translate_callback top (name, ret, args) attrs in
               { defs with callbacks = QNameMap.add c.name c defs.callbacks }
         | SimpleAst.DefInterface i ->
             let i = translate_interface top i attrs in
               { defs with interfaces = QNameMap.add i.name i defs.interfaces }
         | SimpleAst.DefCallbackInterface i ->
             let i = translate_interface top i attrs in
               { defs with callback_interfaces =
                   QNameMap.add i.name i defs.callback_interfaces }
         | SimpleAst.DefDictionary d ->
             let d = translate_dictionary top d attrs in
               { defs with dictionaries = QNameMap.add d.name d defs.dictionaries }
         | SimpleAst.DefImplements (lower, upper) ->
             { defs with implements = (lower, upper) :: defs.implements }
         | SimpleAst.DefTypedef _ ->
             error top "Typedef in definitions; please clean up first"; defs
         | SimpleAst.DefModule _ ->
             error top "Module in definitions; please clean up first"; defs
      ) { dictionaries = QNameMap.empty;
          enumerations = QNameMap.empty;
          interfaces = QNameMap.empty;
          exceptions = QNameMap.empty;
          callbacks = QNameMap.empty;
          callback_interfaces = QNameMap.empty;
          implements = [] }
    *)
let translate_definitions = raise Exit
