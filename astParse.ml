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

let intersect l1 l2 = List.filter (fun x -> List.mem x l1) l2

let map_fst f (x, y) = (f x, y)

let rec translate_type ctx attrs = function
  | SimpleAst.TBoolean -> (BooleanType, attrs)
  | SimpleAst.TByte -> (ByteType, attrs)
  | SimpleAst.TOctet -> (OctetType, attrs)
  | SimpleAst.TObject -> (ObjectType, attrs)
  | SimpleAst.TDate -> (DateType, attrs)
  | SimpleAst.TVoid -> (VoidType, attrs)
  | SimpleAst.TAny -> (AnyType, attrs)
  | SimpleAst.TNamed (SimpleAst.NameBuiltin "string")
  | SimpleAst.TString ->
      let (string_attrs, user_attrs) = parse_string_args ctx attrs in
        (DOMStringType string_attrs, user_attrs)
  | SimpleAst.TNamed (SimpleAst.NameBuiltin t) ->
      ContextError.error ctx "Trying to translate unknown built-in type %s" t;
      (AnyType, attrs)
  | SimpleAst.TNamed (SimpleAst.NamePath n) -> (NamedType n, attrs)
  | SimpleAst.TOptional t ->
      let (treat_undefined_as_missing, remaining_attrs) =
        handle_non_failing_known false ctx [
          xattr_equals_specific "TreatUndefinedAs" [
            "Missing", update_if_default "undefined handling" false true ctx
          ]
        ] attrs
      in let (type_, user_attrs) = translate_type ctx remaining_attrs t
      in (OptionType(treat_undefined_as_missing, type_), user_attrs)
  | SimpleAst.TSequence t ->
      map_fst (fun t' -> SequenceType t') (translate_type ctx attrs t)
  | SimpleAst.TArray t ->
      map_fst (fun t' -> ArrayType t') (translate_type ctx attrs t)
  | SimpleAst.TUnion ts ->
      let (types, user_attrs) = List.split (List.map (translate_type ctx attrs) ts)
      in ((UnionType types), List.fold_left intersect attrs user_attrs)
  | SimpleAst.TInt { Common.length; unsigned } ->
      let (out_of_range, user_attrs) = parse_int_args ctx attrs
      in ((IntType { length; unsigned; out_of_range }), user_attrs)
  | SimpleAst.TFloat ft -> ((FloatType ft), attrs)


let translate_value ctx (x: Common.value): value = x

type ('a, 'b) either = Left of 'a | Right of 'b
let rec partition_map f = function
  | [] -> ([], [])
  | x::l -> let (ll, lr) = partition_map f l in match f x with
      | Left y -> (y::ll, lr)
      | Right y -> (ll, y::lr)

let rec translate_extended_attribute ctx { name; equals; arguments } =
  match equals, arguments with
    | None, None -> UAPlain name
    | Some equals, None -> UAEquals (name, equals)
    | None, Some arguments -> UAArguments (name, translate_arguments ctx arguments)
    | Some equals, Some arguments ->
        UAArgumentsEquals (name, equals, translate_arguments ctx arguments)
and translate_extended_attributes ctx attrs =
  List.map (translate_extended_attribute ctx) attrs
and translate_argument ctx attrs = function
  | SimpleAst.ArgOptional { type_; name; default = None } ->
      let ctx = ctx_push_scope ctx "Argument %s" name in
      let (types, attrs) = translate_type ctx attrs type_ in
      { name; kind=Optional; types;
        user_attributes = translate_extended_attributes ctx attrs }
  | SimpleAst.ArgOptional { type_; name; default = Some value } ->
      let ctx = ctx_push_scope ctx "Argument %s" name in
      let (types, attrs) = translate_type ctx attrs type_ in
      { name; kind=Default value; types;
        user_attributes = translate_extended_attributes ctx attrs }
  | SimpleAst.ArgRequired { type_; name; multiple = false } ->
      let ctx = ctx_push_scope ctx "Argument %s" name in
      let (types, attrs) = translate_type ctx attrs type_ in
      { name; kind=Single; types;
        user_attributes = translate_extended_attributes ctx attrs }
  | SimpleAst.ArgRequired { type_; name; multiple = true } ->
      let ctx = ctx_push_scope ctx "Argument %s" name in
      let (types, attrs) = translate_type ctx attrs type_ in
      { name; kind=Multiple; types;
        user_attributes = translate_extended_attributes ctx attrs }
and translate_arguments ctx =
  List.map (fun (arg, attrs) -> translate_argument ctx attrs arg)

let translate_name ctx = function
  | NamePath p -> p
  | NameBuiltin b ->
      ContextError.error ctx "Trying to define built-in %s" b;
      []

let translate_global_constant ctx ({ type_; name; value }: SimpleAst.global_const) attrs:
      global_constant =
  let ctx = ctx_push_scope ctx "Constant %a" SimpleAst.pp_scoped_name name in
  let (types, user_attributes) = translate_type ctx attrs type_ in
    { name = translate_name ctx name;
      types; user_attributes = translate_extended_attributes ctx user_attributes;
      value = translate_value ctx value }

let translate_constant ctx { SimpleAst.type_; name; value } attrs: constant = 
  let ctx = ctx_push_scope ctx "Constant %s" name in
  let (types, user_attributes) = translate_type ctx attrs type_ in
  { name; types; user_attributes = translate_extended_attributes ctx user_attributes;
    value = translate_value ctx value }

let translate_attribute ctx { SimpleAst.inherited; readonly; type_; name; get; set } attrs =
  let ctx = ctx_push_scope ctx "Attribute %s" name
  and update_access = update_if_default "Access mode" ReadOnly in
  let ((lenient_this, access), attrs) =
    handle_all_known (false, if readonly then ReadOnly else ReadWrite) ctx [
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
  let (types, attrs) = translate_type ctx attrs type_ in
  { name; lenient_this; inherited; access; types;
    user_attributes = translate_extended_attributes ctx attrs }

let translate_return_type ctx return attrs =
  let ctx = ctx_push_scope ctx "Return type" in
    translate_type ctx 
      (drop_bad_attributes ctx ["TreatNullAs"; "TreatUndefinedAs"] attrs)
      return

let translate_regular_operation ctx (name: string option) return args attrs: 
      IdlData.operation = 
  let name = match name with
    | Some name -> name 
    | None -> error ctx "Unnamed regular operation"; "???"
  in
  let ctx = ctx_push_scope ctx "Operation %s" name in
  let (return, attrs) = translate_return_type ctx return attrs in
    { name; return; user_attributes = translate_extended_attributes ctx attrs;
      args = translate_arguments ctx args
    }

let without_name = Fmt.const Fmt.string " (without name)"

let translate_legacy_caller ctx name return' args attrs =
  let ctx = ctx_push_scope ctx "Legacy operation %a"
              (Fmt.option ~none:without_name Fmt.string) name in
  let (return, attrs) = translate_return_type ctx return' attrs in
  ( { return;
      args = translate_arguments ctx args;
      user_attributes = translate_extended_attributes ctx attrs
  },
    match name with
      | Some _ -> [ translate_regular_operation ctx name return' args attrs ]
      | None  -> [])

let translate_special
      ctx qualifier name return (args: SimpleAst.arguments)
      attrs indexed_properties named_properties =
  let ctx = ctx_push_scope ctx "Special operation %a"
              (Fmt.option ~none:without_name Fmt.string) name in
  let translate_attributed_type translate ctx ty attrs =
    let (types, attrs) = translate ctx ty attrs in
      { types; user_attributes = translate_extended_attributes ctx attrs }
  in let translate_one return attrs = 
    Some (translate_attributed_type translate_return_type ctx return attrs)
  and translate_two arg argattrs return retattrs =
    Some (translate_attributed_type translate_return_type ctx return retattrs,
          translate_attributed_type (fun ctx ty attrs -> translate_type ctx attrs ty)
            ctx arg argattrs)
  in
  ((match qualifier,
          List.map (function
                      | SimpleAst.ArgOptional { type_ }, attrs
                      | SimpleAst.ArgRequired { type_ }, attrs -> (type_, attrs))
            args with
    | SimpleAst.QGetter,
      [ (SimpleAst.TInt { Common.length = Common.ILong; unsigned = true }, argattrs) ] ->
        if argattrs <> [] then warn ctx "Attributes given for index argument";
        ({ indexed_properties with getter = translate_one return attrs },
         named_properties)
    | SimpleAst.QGetter, [ (SimpleAst.TString, argattrs) ] ->
        if argattrs <> [] then warn ctx "Attributes given for index argument";
        (indexed_properties,
         { named_properties with getter = translate_one return attrs })
    | SimpleAst.QDeleter,
      [ (SimpleAst.TInt { Common.length = Common.ILong; unsigned = true }, argattrs) ] ->
        if argattrs <> [] then warn ctx "Attributes given for index argument";
        ({ indexed_properties with deleter = translate_one return attrs },
         named_properties)
    | SimpleAst.QDeleter, [ (SimpleAst.TString, argattrs) ] ->
        if argattrs <> [] then warn ctx "Attributes given for index argument";
        (indexed_properties,
         { named_properties with deleter = translate_one return attrs })
    | SimpleAst.QSetter,
      [ (SimpleAst.TInt { Common.length = Common.ILong; unsigned = true }, retattrs);
                        (arg, argattrs) ] ->
        ({ indexed_properties with setter = translate_two arg argattrs return retattrs },
         named_properties)
    | SimpleAst.QSetter, [ (SimpleAst.TString, retattrs); (arg, argattrs) ] ->
        (indexed_properties,
         { named_properties with setter = translate_two arg argattrs return retattrs })
    | SimpleAst.QCreator,
      [ (SimpleAst.TInt { Common.length = Common.ILong; unsigned = true }, retattrs);
                        (arg, argattrs) ] ->
        ({ indexed_properties with creator = translate_two arg argattrs return retattrs },
         named_properties)
    | SimpleAst.QCreator, [ (SimpleAst.TString, retattrs);
                         (arg, argattrs) ] ->
        (indexed_properties,
         { named_properties with creator = translate_two arg argattrs return retattrs })
    | _, _ -> error ctx "Unparsable special operation";
              (indexed_properties, named_properties)),
   match name with
     | Some _ ->
         let args = List.map
                      (function
                         | (SimpleAst.ArgOptional { type_; name }, attrs)
                         | (SimpleAst.ArgRequired { type_; name }, attrs) ->
                             (SimpleAst.ArgRequired { type_; name; multiple = false },
                              attrs))
                      args
         in [ translate_regular_operation ctx name return args attrs ]
     | None -> [])

let translate_operation ctx name return arguments raises attrs interface =
  { interface with operations =
      translate_regular_operation ctx name return arguments attrs ::
                   interface.operations }

let translate_qualified_operation ctx name return arguments raises qualifier attrs
      interface =
  match qualifier with
    | SimpleAst.QOmittable ->
        translate_operation ctx name return arguments raises attrs interface
    | SimpleAst.QLegacyCaller ->
        let (legacy, op) = translate_legacy_caller ctx name return arguments attrs
        in { interface with operations = op @ interface.operations;
                            legacy_callers = legacy :: interface.legacy_callers }
    | SimpleAst.QStatic ->
        let op = translate_regular_operation ctx name return arguments attrs
        in { interface with static_operations = op :: interface.static_operations }
    | _ ->
        let ((indexed_properties, named_properties), op) =
          translate_special ctx qualifier name return arguments attrs
            interface.indexed_properties interface.named_properties
        in { interface with indexed_properties; named_properties;
                            operations = op @ interface.operations }

let translate_stringifer_empty ctx attrs =
  let ctx = ctx_push_scope ctx "Anonymous stringifier" in
  let (string_mode, attrs) = parse_string_args ctx attrs in
  InternalStringifer (string_mode, translate_extended_attributes ctx attrs)

let translate_stringifier_operation ctx (o: SimpleAst.operation) attrs =
  let ctx = ctx_push_scope ctx "Stringifier operation %a"
              (Fmt.option Fmt.string) o.name in
  begin if o.return_type <> SimpleAst.TString then
    warn ctx "Stringifier does not return string"
  end;
  begin if o.arguments <> [] then
    warn ctx "Stringifier takes arguments"
  end;
  let (string_mode, attrs) = parse_string_args ctx attrs in
  ( InternalStringifer (string_mode, translate_extended_attributes ctx attrs),
    match o.name with
      | Some _ -> [ translate_regular_operation ctx o.name o.return_type o.arguments attrs ]
      | None -> [])

let translate_stringifier_attribute ctx name inherited readonly types attrs =
  let ctx = ctx_push_scope ctx "Stringifier attribute %s" name in
  let (string_mode, attrs) = parse_string_args ctx attrs in
  ( AttributeStringifier (name, string_mode, translate_extended_attributes ctx attrs),
    translate_attribute ctx
      { name; inherited; readonly; type_ = types; get = GRaises []; set = [] } attrs)

let translate_member ctx interface (member, attrs) =
  let update_stringifier stringifier =
    if interface.stringifier <> NoStringifier then warn ctx "More than one stringifier";
    { interface with stringifier }
  in match member with
    | SimpleAst.IStringifier SimpleAst.StringBare -> 
        { interface with stringifier = translate_stringifer_empty ctx attrs }
    | SimpleAst.IStringifier (SimpleAst.StringOperation o) ->
        let (str, op) = translate_stringifier_operation ctx o attrs in
        let interface' = update_stringifier str in
          { interface' with operations = op @ interface.operations }
    | SimpleAst.IStringifier
        (SimpleAst.StringAttribute {name; inherited; readonly; type_}) ->
        let (str, attr) =
          translate_stringifier_attribute ctx name inherited readonly type_ attrs in
        let interface' = update_stringifier str in
          { interface' with attributes = attr :: interface.attributes }
    | SimpleAst.IOperation {name; return_type; arguments; raises } ->
        translate_operation ctx name return_type arguments raises attrs interface
    | SimpleAst.ISpecialOperation (q, {name; return_type; arguments; raises}) ->
        translate_qualified_operation
          ctx name return_type arguments raises q attrs interface
    | SimpleAst.IAttribute ad ->
        let attr = translate_attribute ctx ad attrs in
          { interface with attributes = attr :: interface.attributes }
    | SimpleAst.IConst cd ->
        let value = translate_constant ctx cd attrs in
          { interface with consts = value :: interface.consts }

let parse_constructor ctx name constructors (args: SimpleAst.arguments option) =
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

let translate_interface ctx ({ name; inheritance; members }: SimpleAst.interface) attrs =
  let ctx = ctx_push_scope ctx "Interface %a" pp_scoped_name name in
  let name = translate_name ctx name in
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
  in let inheritance =
    if is_array then
      ["Array"] :: List.map (translate_name ctx) inheritance
    else
      List.map (translate_name ctx) inheritance
  and empty_properties = { getter = None; setter = None; deleter = None; creator = None }
  in let init = {
    inheritance; name; not_exposed; constructors;
    user_attributes = translate_extended_attributes ctx attrs;
    special = { this_lenient; this_implicit; named_properties_object; override_builtins };
    consts = []; attributes = []; operations = [];
    static_operations = [];
    named_properties = empty_properties;
    indexed_properties = empty_properties;
    legacy_callers = [];
    stringifier = NoStringifier
  }
  in List.fold_left (translate_member ctx) init members


let translate_dictionary_entry ctx
      (({name; type_; default}: SimpleAst.dictionary_member), attrs) =
  let ctx = ctx_push_scope ctx "Dictionary entry %s" name in
  let (types, attrs) =
    translate_type ctx (keep_good_attributes ["Clamp"; "EnforceRange"] attrs) type_ in
  { name; types;
    user_attributes = translate_extended_attributes ctx attrs;
    default_value = BatOption.map (translate_value ctx) default
  }

let translate_dictionary ctx ({name; inheritance; members}: SimpleAst.dictionary) attrs =
  let ctx = ctx_push_scope ctx "Dictionary %a" pp_scoped_name name
  in let name = translate_name ctx name in
  { name; inherits_from = List.map (translate_name ctx) inheritance;
    user_attributes = translate_extended_attributes ctx attrs;
       members = List.map (translate_dictionary_entry ctx) members
  }

let translate_exception_member ctx name types attrs =
  let ctx = ctx_push_scope ctx "Exception member %s" name in
  let (types, attrs) = translate_type ctx types attrs in
  { name; types; user_attributes = translate_extended_attributes ctx attrs }

let translate_exception_member' ctx exc (member, attrs) = match member with
  | SimpleAst.EConst const ->
      { exc with consts = translate_constant ctx const attrs :: exc.consts }
  | SimpleAst.EField { name; type_ } ->
      { exc with
            members = translate_exception_member ctx name attrs type_ :: exc.members }

let translate_exception ctx ({ name; inheritance; members }: SimpleAst.exception_) attrs =
  let ctx = ctx_push_scope ctx "Exception %a" pp_scoped_name name in
  let (not_exposed, attrs) =
    handle_all_known false ctx [
      xattr_plain "NoInterfaceObject" (update_if_default "NoInterfaceObject" false true)
    ] attrs
  in List.fold_left (translate_exception_member' ctx)
       { name = translate_name ctx name;
         inherits_from = List.map (translate_name ctx) inheritance;
         not_exposed;
         user_attributes = translate_extended_attributes ctx attrs;
         consts = [];
         members = [];
       } members

let translate_enumeration ctx { name; contents } attrs =
  let ctx = ctx_push_scope ctx "Enumeration %a" pp_scoped_name name in
  { name = translate_name ctx name;
    values = contents;
    user_attributes = translate_extended_attributes ctx attrs }

let translate_callback ctx { name; type_; arguments } attrs =
  let ctx = ctx_push_scope ctx "Callback %a" pp_scoped_name name in
  let (treat_non_callable_as_null, attrs) =
    handle_all_known false ctx [
      xattr_plain "TreatNonCallableAsNull"
        (update_if_default "TreatNonCallableAsNull" false true)
    ] attrs
  in let (return, _) = translate_return_type ctx (fst type_) (snd type_)
  and args = translate_arguments ctx arguments
  and name = translate_name ctx name
  in { name; return; args; treat_non_callable_as_null;
       user_attributes = translate_extended_attributes ctx attrs }

let translate_callback_interface ctx desc attrs =
  translate_interface ctx desc
    (drop_bad_attributes ctx
       ["Constructor"; "NamedConstructor"; "NoInterfaceObject"] attrs)

let translate_definition ctx defs = function
  | SimpleAst.DCallbackInterface (desc, attrs) ->
      let x = translate_callback_interface ctx desc attrs in
        { defs with callback_interfaces = QNameMap.add x.name x defs.callback_interfaces }
  | SimpleAst.DInterface (desc, attrs) ->
      let x = translate_interface ctx desc attrs in
        { defs with interfaces = QNameMap.add x.name x defs.interfaces }
  | SimpleAst.DCallback (desc, attrs) ->
      let x = translate_callback ctx desc attrs in
        { defs with callbacks = QNameMap.add x.name x defs.callbacks }
  | SimpleAst.DDictionary (desc, attrs) ->
      let x = translate_dictionary ctx desc attrs in
        { defs with dictionaries = QNameMap.add x.name x defs.dictionaries }
  | SimpleAst.DException (desc, attrs) ->
      let x = translate_exception ctx desc attrs in
        { defs with exceptions = QNameMap.add x.name x defs.exceptions }
  | SimpleAst.DEnum (desc, attrs) ->
      let x = translate_enumeration ctx desc attrs in
        { defs with enumerations = QNameMap.add x.name x defs.enumerations }
  | SimpleAst.DImplements (l, r) ->
      { defs with implements =
          (translate_name ctx l, translate_name ctx r) :: defs.implements }
  | SimpleAst.DConst desc ->
      let x = translate_global_constant ctx desc [] in
      { defs with constants = QNameMap.add x.name x defs.constants }

let translate_definitions =
  let top = ctx_top () in
  let result = List.fold_left (translate_definition top)
      { dictionaries = QNameMap.empty;
        enumerations = QNameMap.empty;
        interfaces = QNameMap.empty;
        exceptions = QNameMap.empty;
        callbacks = QNameMap.empty;
        callback_interfaces = QNameMap.empty;
        constants = QNameMap.empty;
        implements = [] }
  in ContextError.flush_errors_and_handle_failure top; result
