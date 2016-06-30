open SimpleAst
(** First step: Resolve modules, partials and typedefs. *)                     
module ScopedName = struct
  type t = scoped_name = NamePath of string list | NameBuiltin of string
        [@@deriving ord,eq,show]
end
module ScopedNameMap = BatMap.Make(ScopedName)

(* Step 1: Resolve modules and split out partials and typedefs. *)
type module_resolution = {
  mr_definitions: definition list;
  mr_typedefs: type_ with_attributes ScopedNameMap.t;
  mr_partial_interface: interface list ScopedNameMap.t;
  mr_partial_dictionary: dictionary list ScopedNameMap.t
}

let translate_name ctx prefix name =
  if name = "DOMString" then
    (* Crazy stuff may happen here. FIXME: Do this properly. *)
    NameBuiltin "DOMString"
  else
      NamePath (prefix @ [name])

let translate_scoped_name ctx prefix { Ast.absolute; Ast.ends_in_domstring; Ast.path } =
  if ends_in_domstring then
    NameBuiltin "DOMString"
  else if absolute then
    NamePath path
  else match path with
    | _ :: _ :: _ ->
        (* Needs checking - the absolute name rules are not quite consistent, it seems. *)
        NamePath path
    | _ -> NamePath (prefix @ path)

let rec translate_type ctx prefix = function
  | Ast.TSequence t -> TSequence (translate_type ctx prefix t)
  | Ast.TArray t -> TArray (translate_type ctx prefix t)
  | Ast.TOptional t -> TOptional (translate_type ctx prefix t)
  | Ast.TUnion t -> TUnion (List.map (translate_type ctx prefix) t)
  | Ast.TNamed t -> begin match translate_scoped_name ctx prefix t with
      | NameBuiltin "DOMString" -> TString
      | n -> TNamed n
    end
  | Ast.TBoolean -> TBoolean
  | Ast.TByte -> TByte
  | Ast.TString -> TString
  | Ast.TObject -> TObject
  | Ast.TDate -> TDate
  | Ast.TVoid -> TVoid
  | Ast.TAny -> TAny
  | Ast.TOctet -> TOctet
  | Ast.TInt it -> TInt it
  | Ast.TFloat ft -> TFloat ft

let translate_global_const ctx prefix mr { Ast.type_; Ast.name; Ast.value } =
  let co: global_const = 
    { type_ = translate_type ctx prefix type_;
      name = translate_name ctx prefix name;
      value }
  in { mr with mr_definitions = DConst co :: mr.mr_definitions }

let translate_const ctx prefix { Ast.type_; Ast.name; Ast.value }: const =
  { type_ = translate_type ctx prefix type_;
    name; value }

let translate_get ctx prefix = function
  | Ast.GRaises exc -> GRaises (List.map (translate_scoped_name ctx prefix) exc)
  | Ast.GInherits -> GInherits

let translate_attribute ctx prefix
      { Ast.inherited; Ast.readonly; Ast.type_; Ast.name; Ast.get; Ast.set } =
  { inherited; readonly; name;
    type_ = translate_type ctx prefix type_;
    get = translate_get ctx prefix get;
    set = List.map (translate_scoped_name ctx prefix) set }

let rec translate_arguments ctx prefix args =
  List.map (translate_with_attributes_inner translate_argument ctx prefix) args
and translate_argument ctx prefix =
  let open Ast in function
  | ArgOptional { type_; name; default } ->
      SimpleAst.ArgOptional { type_ = translate_type ctx prefix type_; name; default }
  | ArgRequired { type_; name; multiple } ->
      SimpleAst.ArgRequired { type_ = translate_type ctx prefix type_; name; multiple }
and translate_with_attributes_inner
        translate_first
        ctx prefix
        ((first, attr): 'a * Ast.extended_attribute_list):
                               'b * extended_attribute_list =
    (translate_first ctx prefix first, translate_extended_attribute_list ctx prefix attr)
and translate_extended_attribute_list ctx prefix attrs =
  List.map (translate_extended_attribute ctx prefix) attrs
and translate_extended_attribute ctx prefix { Ast.name; Ast.equals; Ast.arguments } =
  { name; equals; arguments = BatOption.map (translate_arguments ctx prefix) arguments }

let translate_with_attributes translate_first ctx prefix (first, attr) =
  (translate_first ctx prefix first, translate_extended_attribute_list ctx prefix attr)

let translate_operation ctx prefix { Ast.return_type; Ast.name; Ast.arguments; Ast.raises } =
  { return_type = translate_type ctx prefix return_type;
    name;
    arguments = translate_arguments ctx prefix arguments;
    raises = List.map (translate_scoped_name ctx prefix) raises }

let translate_interface_member ctx prefix = function
  | Ast.IConst c -> IConst (translate_const ctx prefix c)
  | Ast.IAttributeOrOperation (Ast.Attribute a) ->
      IAttribute (translate_attribute ctx prefix a)
  | Ast.IAttributeOrOperation (Ast.Operation (None, o)) ->
      IOperation (translate_operation ctx prefix o)
  | Ast.IAttributeOrOperation (Ast.Operation (Some q, o)) ->
      ISpecialOperation (q, translate_operation ctx prefix o)
  | Ast.IAttributeOrOperation (Ast.Stringifier (Ast.StringBare)) ->
      IStringifier StringBare
  | Ast.IAttributeOrOperation (Ast.Stringifier (Ast.StringAttribute a)) ->
      IStringifier (StringAttribute (translate_attribute ctx prefix a))
  | Ast.IAttributeOrOperation (Ast.Stringifier (Ast.StringOperation o)) ->
      IStringifier (StringOperation (translate_operation ctx prefix o))

let translate_inheritance ctx prefix names = List.map (translate_scoped_name ctx prefix) names

let translate_regular_interface_impl ctx prefix
      ({ Ast.name; Ast.inheritance; Ast.members }: Ast.regular_interface): interface =
  { name = translate_name ctx prefix name;
    inheritance = translate_inheritance ctx prefix inheritance;
    members =
      List.map (translate_with_attributes translate_interface_member ctx prefix) members }

let translate_callback_interface ctx prefix ({ mr_definitions } as mr) it attr =
  let it' = translate_regular_interface_impl ctx prefix it
  and attr' = translate_extended_attribute_list ctx prefix attr in
    { mr with mr_definitions = DCallbackInterface (it', attr') :: mr_definitions }

let translate_regular_interface ctx prefix ({ mr_definitions } as mr) it attr =
  let it' = translate_regular_interface_impl ctx prefix it
  and attr' = translate_extended_attribute_list ctx prefix attr in
    { mr with mr_definitions = DInterface (it', attr') :: mr_definitions }

let translate_forward_interface ctx prefix ({ mr_partial_interface } as mr) name attr =
  if attr <> [] then
    prerr_endline "Attributes given for interface forward definition";
  let full_name = translate_name ctx prefix name in
  if ScopedNameMap.mem full_name mr_partial_interface then
    mr
  else
    { mr with mr_partial_interface = ScopedNameMap.add full_name [] mr_partial_interface }

let translate_partial_interface_impl
      ctx prefix ({ Ast.name; Ast.members }: Ast.partial_interface) =
  translate_regular_interface_impl ctx prefix
    { Ast.name; Ast.members; Ast.inheritance = [] }

let translate_partial_interface ctx prefix ({ mr_partial_interface } as mr)
      (it: Ast.partial_interface) attr =
  if attr <> [] then
    prerr_endline "Attribute sgiven for partial interface definition";
  let full_name = translate_name ctx prefix it.Ast.name in
    { mr with mr_partial_interface =
        ScopedNameMap.modify_def [] full_name
          (fun defs -> translate_partial_interface_impl ctx prefix it :: defs)
          mr_partial_interface }

let translate_dictionary_member ctx prefix { Ast.type_; Ast.name; Ast.default } =
  { type_ = translate_type ctx prefix type_;
    name; default }

let translate_dictionary ctx prefix mr
      { Ast.partial; Ast.name; Ast.inheritance; Ast.members } attr =
  let di': dictionary =
    { name = translate_name ctx prefix name;
      inheritance = translate_inheritance ctx prefix inheritance;
      members = List.map (translate_with_attributes translate_dictionary_member ctx prefix)
                  members }
  and attr' = translate_extended_attribute_list ctx prefix attr
  in if partial then begin
    if attr' <> [] then
      prerr_endline "Non-empty attributes for a partial interface";
    { mr with mr_partial_dictionary = ScopedNameMap.modify_def [] di'.name
                                        (fun dis -> di' :: dis) mr.mr_partial_dictionary }
  end else
    { mr with mr_definitions = DDictionary (di', attr') :: mr.mr_definitions }

let translate_callback ctx prefix mr { Ast.name; Ast.type_; Ast.arguments } attr =
  let cb =
    { name = translate_name ctx prefix name;
      type_ = translate_with_attributes translate_type ctx prefix type_;
      arguments = translate_arguments ctx prefix arguments }
  and attr' = translate_extended_attribute_list ctx prefix attr
  in { mr with mr_definitions = DCallback (cb, attr') :: mr.mr_definitions }

let translate_exception_member ctx prefix = function
  | Ast.EConst const -> EConst (translate_const ctx prefix const)
  | Ast.EField { Ast.type_; Ast.name } ->
      EField { name; type_ = translate_type ctx prefix type_ }

let translate_exception ctx prefix mr
      ({ Ast.name; Ast.inheritance; Ast.members }: Ast.exception_) attr =
  let ex: exception_ =
    { name = translate_name ctx prefix name;
      inheritance = translate_inheritance ctx prefix inheritance;
      members =
        List.map (translate_with_attributes translate_exception_member ctx prefix) members }
  and attr' = translate_extended_attribute_list ctx prefix attr
  in { mr with mr_definitions = DException (ex, attr') :: mr.mr_definitions }

let translate_enum ctx prefix mr { Ast.name; Ast.contents } attr =
  { mr with mr_definitions =
      DEnum ({ name = translate_name ctx prefix name; contents },
             translate_extended_attribute_list ctx prefix attr) :: mr.mr_definitions }

let translate_implements ctx prefix mr (l, r) =
  { mr with mr_definitions =
      DImplements (translate_scoped_name ctx prefix l, translate_scoped_name ctx prefix r) ::
            mr.mr_definitions }

let translate_typedef ctx prefix mr ({ Ast.name; Ast.type_ }: Ast.typedef) =
  let full_name = translate_name ctx prefix name
  and type_ = translate_with_attributes translate_type ctx prefix type_ in
    if ScopedNameMap.mem full_name mr.mr_typedefs then begin
      begin if ScopedNameMap.find full_name mr.mr_typedefs = type_ then
        ContextError.warn ctx "Type name %a defined twice, with the same expansion"
          pp_scoped_name full_name
      else
        ContextError.error ctx "Type name %a defined twice, with conflicting expansions"
          pp_scoped_name full_name
      end;
      mr
    end else
      { mr with mr_typedefs =
          ScopedNameMap.add full_name type_ mr.mr_typedefs }

let rec step1_translate_one (ctx: ContextError.ctx) prefix mr = function
  | Ast.DCallbackInterface (Ast.IRegular it, attr) ->
      translate_callback_interface ctx prefix mr it attr
  | Ast.DCallbackInterface ((Ast.IForward name), _)
  | Ast.DCallbackInterface ((Ast.IPartial { Ast.name }), _) ->
      ContextError.error ctx
        "Encountered callback interface %s that is not a regular interface"
        name;
      mr
  | Ast.DCallback (cb, attr) -> translate_callback ctx prefix mr cb attr
  | Ast.DInterface (Ast.IRegular it, attr) ->
      translate_regular_interface ctx prefix mr it attr
  | Ast.DInterface (Ast.IForward name, attr) ->
      translate_forward_interface ctx prefix mr name attr
  | Ast.DInterface (Ast.IPartial it, attr) ->
      translate_partial_interface ctx prefix mr it attr
  | Ast.DDictionary (di, attr) -> translate_dictionary ctx prefix mr di attr
  | Ast.DException (ex, attr) -> translate_exception ctx prefix mr ex attr
  | Ast.DEnum (en, attr) -> translate_enum ctx prefix mr en attr
  | Ast.DImplements im -> translate_implements ctx prefix mr im
  | Ast.DConst co -> translate_global_const ctx prefix mr co
  | Ast.DModule { Ast.name; Ast.definitions } ->
      step1_translate_all ctx (prefix @ [name]) mr definitions
  | Ast.DTypedef ty -> translate_typedef ctx prefix mr ty
  | Ast.DNothing -> mr
and step1_translate_all ctx prefix mr = function
  | def::defs -> step1_translate_all ctx prefix (step1_translate_one ctx prefix mr def) defs
  | [] -> mr

let step1 ctx defs =
  step1_translate_all ctx []
    { mr_definitions = [];
      mr_typedefs = ScopedNameMap.empty;
      mr_partial_interface = ScopedNameMap.empty;
      mr_partial_dictionary = ScopedNameMap.empty }
    defs

(* Step 2: Resolve partial structures. *)
type structure_resolution = {
  sr_definitions: definition list;
  sr_typedefs: type_ with_attributes ScopedNameMap.t
}

let rec step2_impl ctx partial_dictionaries partial_interfaces defs' = function
  | DInterface (({name; members} as it), attrs) :: defs ->
      begin try
        let (partials, partial_interfaces') =
          ScopedNameMap.extract name partial_interfaces
        in let members' = List.fold_left (fun members' { members } -> members @ members')
                            members partials
        in let it' = ({ it with members = members' }, attrs)
        in step2_impl ctx partial_dictionaries partial_interfaces'
             (DInterface it' :: defs') defs
      with Not_found ->
        step2_impl ctx partial_dictionaries partial_interfaces
          (DInterface (it, attrs) :: defs') defs
      end
  | DDictionary (({name; members} as di), attrs) :: defs ->
      begin try
        let ((partials: dictionary list), partial_dictionaries') =
          ScopedNameMap.extract name partial_dictionaries
        in let members' = List.fold_left (fun members' ({ members }: dictionary) ->
                                            members @ members')
                            members partials
        in let di' = ({ di with members = members' }, attrs)
        in step2_impl ctx partial_dictionaries' partial_interfaces
             (DDictionary di' :: defs') defs
      with Not_found ->
        step2_impl ctx partial_dictionaries partial_interfaces
          (DDictionary (di, attrs) :: defs') defs
      end
  | def :: defs ->
      step2_impl ctx partial_dictionaries partial_interfaces (def :: defs') defs
  | [] ->
      if not (ScopedNameMap.is_empty partial_dictionaries) then
        ContextError.error ctx
          "The following partial dictionaries have not been resolved: @[<hov>%a@]@."
          (Fmt.list pp_scoped_name) (ScopedNameMap.keys partial_dictionaries |> BatList.of_enum);
      if not (ScopedNameMap.is_empty partial_interfaces) then
        ContextError.error ctx
          "The following partial interfaces have not been resolved: @[<hov>%a@]@."
          (Fmt.list pp_scoped_name) (ScopedNameMap.keys partial_interfaces |> BatList.of_enum);
      defs'

let step2 ctx { mr_definitions; mr_typedefs; mr_partial_interface; mr_partial_dictionary } =
  { sr_definitions = step2_impl ctx mr_partial_dictionary mr_partial_interface [] mr_definitions;
    sr_typedefs = mr_typedefs }

(* Step 3: Resolve typedefs. *)
module Vertex = struct
  include ScopedName
  let hash (x: t) = Hashtbl.hash x
end
module G = Graph.Imperative.Digraph.Concrete(Vertex)
module GTopo = Graph.Topological.Make(G)

let rec iter_type_referenced_names f = function
  | TUnion ts -> List.iter (iter_type_referenced_names f) ts
  | TSequence t | TArray t | TOptional t -> iter_type_referenced_names f t
  | TNamed n -> f n
  | _ -> ()

let build_reference_graph typedefs =
  let depgraph = G.create () in
    ScopedNameMap.iter (fun new_name type_ ->
                          iter_type_referenced_names
                            (fun old_name -> G.add_edge depgraph new_name old_name)
                            type_)
      typedefs;
    depgraph

let rec type_substitute_named_types ctx subst = function
  | TNamed name as t ->
      begin try ScopedNameMap.find name subst with Not_found -> t end
  | TSequence t -> TSequence (type_substitute_named_types ctx subst t)
  | TArray t -> TArray (type_substitute_named_types ctx subst t)
  | TOptional t -> TOptional (type_substitute_named_types ctx subst t)
  | TUnion ts -> TUnion (List.map (type_substitute_named_types ctx subst) ts)
  | t -> t

let build_typedef_map ctx typedefs =
  let typedefs =
    ScopedNameMap.map
      (fun (type_, attrs) ->
         if attrs <> [] then
           prerr_endline "Extended attributes in typedef, ignoring";
         type_) typedefs
  in let g = build_reference_graph typedefs in
    GTopo.fold (fun typename subst -> try
                  let type_ = ScopedNameMap.find typename typedefs
                  in ScopedNameMap.add typename (type_substitute_named_types ctx subst type_)
                       subst
                with Not_found -> subst)
      g ScopedNameMap.empty

let type_substitute_type_name ctx subst name =
  match ScopedNameMap.Exceptionless.find name subst with
    | Some (TNamed name') -> name'
    | None -> name
    | Some type_ ->
        ContextError.error ctx
          "In a context where simple types are required, typedef %a is expanded to %a"
          pp_scoped_name name pp_type_ type_;
        name

let type_substitute_inheritance ctx subst: inheritance -> inheritance =
  BatList.map (type_substitute_type_name ctx subst)

let type_substitute_raises ctx subst = BatList.map (type_substitute_type_name ctx subst)

let rec type_substitute_extended_attribute ctx subst { name; equals; arguments } =
  { name; equals; arguments = BatOption.map (type_substitute_arguments ctx subst) arguments }
and type_substitute_extended_attribute_list ctx subst attrs =
  BatList.map (type_substitute_extended_attribute ctx subst) attrs
and type_substitute_arguments ctx subst =
  BatList.map (fun (arg, attr) ->
                (type_substitute_argument ctx subst arg,
                 type_substitute_extended_attribute_list ctx subst attr))
and type_substitute_argument ctx subst = function
  | ArgOptional { type_; name; default } ->
      ArgOptional { name; default; type_ = type_substitute_named_types ctx subst type_ }
  | ArgRequired { type_; name; multiple } ->
      ArgRequired { name; multiple; type_ = type_substitute_named_types ctx subst type_ }

let type_substitute_with_attributes tsubst ctx subst (x, attr) =
  (tsubst ctx subst x, type_substitute_extended_attribute_list ctx subst attr)

let type_substitute_const ctx subst { type_; name; value } =
  { name; value; type_ = type_substitute_named_types ctx subst type_ }
let type_substitute_attribute ctx subst { inherited; readonly; type_; name; get; set } =
  { inherited; readonly; name;
    type_ = type_substitute_named_types ctx subst type_;
    set = type_substitute_raises ctx subst set;
    get = match get with
      | GRaises ex -> GRaises (type_substitute_raises ctx subst ex)
      | GInherits -> GInherits }
let type_substitute_operation ctx subst { return_type; name; arguments; raises } =
  { name;
    return_type = type_substitute_named_types ctx subst return_type;
    raises = type_substitute_raises ctx subst raises;
    arguments = type_substitute_arguments ctx subst arguments }

let type_substitute_interface ctx subst { name; inheritance; members } =
  let type_substitute_interface_member ctx subst = function
    | IConst co -> IConst (type_substitute_const ctx subst co)
    | IAttribute at -> IAttribute (type_substitute_attribute ctx subst at)
    | IOperation op -> IOperation (type_substitute_operation ctx subst op)
    | ISpecialOperation (q, op) ->
        ISpecialOperation (q, type_substitute_operation ctx subst op)
    | IStringifier (StringAttribute a) ->
        IStringifier (StringAttribute (type_substitute_attribute ctx subst a))
    | IStringifier (StringOperation o) ->
        IStringifier (StringOperation (type_substitute_operation ctx subst o))
    | IStringifier StringBare -> IStringifier StringBare
  in { name;
    inheritance = type_substitute_inheritance ctx subst inheritance;
    members = BatList.map
                (type_substitute_with_attributes type_substitute_interface_member ctx subst)
                members }

let type_substitute_callback ctx subst { name; type_; arguments } =
  { name;
    type_ = type_substitute_with_attributes type_substitute_named_types ctx subst type_;
    arguments = type_substitute_arguments ctx subst arguments }

let type_substitute_dictionary ctx subst ({ name; inheritance; members }: dictionary):
                                                             dictionary =
  let subst_dictionary_member ctx subst ({ type_; name; default }: dictionary_member) =
    { name; default; type_ = type_substitute_named_types ctx subst type_ }
  in
  { name;
    inheritance = type_substitute_inheritance ctx subst inheritance;
    members = BatList.map (type_substitute_with_attributes subst_dictionary_member ctx subst)
                members }

let type_substitute_exception ctx subst ({ name; inheritance; members }: exception_):
                                                            exception_ =
  let subst_exception_member ctx subst = function
    | EConst co -> EConst (type_substitute_const ctx subst co)
    | EField { type_; name } ->
        EField { name; type_ = type_substitute_named_types ctx subst type_ }
  in
    { name;
      inheritance = type_substitute_inheritance ctx subst inheritance;
      members = BatList.map (type_substitute_with_attributes subst_exception_member ctx subst)
                  members }

let type_substitute_global_const ctx subst ({ type_; name; value }: global_const): global_const =
  { name; value; type_ = type_substitute_named_types ctx subst type_ }

let step3 ctx { sr_definitions; sr_typedefs } =
  let subst = build_typedef_map ctx sr_typedefs in
    List.map (function
                | DCallbackInterface it ->
                    DCallbackInterface
                      (type_substitute_with_attributes type_substitute_interface ctx subst it)
                | DCallback cb ->
                    DCallback
                      (type_substitute_with_attributes type_substitute_callback ctx subst cb)
                | DInterface it ->
                    DInterface
                      (type_substitute_with_attributes type_substitute_interface ctx subst it)
                | DDictionary di ->
                    DDictionary
                      (type_substitute_with_attributes type_substitute_dictionary ctx subst di)
                | DException ex ->
                    DException
                      (type_substitute_with_attributes type_substitute_exception ctx subst ex)
                | DEnum en -> DEnum (type_substitute_with_attributes (fun _ _ x -> x) ctx subst en)
                | DImplements (l, r) -> DImplements (l, r)
                | DConst gc -> DConst (type_substitute_global_const ctx subst gc))
      sr_definitions

let cleanup defs =
  let ctx = ContextError.ctx_top () in
  let res = defs |> step1 ctx |> step2 ctx |> step3 ctx in
    ContextError.flush_errors_and_handle_failure ctx;
    res
