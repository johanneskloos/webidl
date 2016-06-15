open Ast
(** First step: Resolve partials and typedefs. *)                     
(* For typedef ordering *)
module Vertex = struct
  type t = string list
  let equal: string list -> string list -> bool = (=)
  let compare l1 l2 = BatEnum.compare BatString.compare (BatList.enum l1) (BatList.enum l2)
  let hash: t -> int = Hashtbl.hash
end
module G = Graph.Imperative.Digraph.Concrete(Vertex)
module StringListMap = BatMap.Make(Vertex)

let rec iter_type_names f = function
  | TypeLeaf (NamedType n) -> f n
  | TypeLeaf _ -> ()
  | TypeUnion l -> List.iter (iter_type_names f) l
  | TypeArray t | TypeOption t | TypeNullable t | TypeSequence t -> iter_type_names f t

let collect_typedef_dependencies depgraph ((name: string list), def, _) =
  iter_type_names (fun name' -> G.add_edge depgraph name' name) def

let collect_dependencies (defs: Ast.typedef_data list) =
  let depgraph = G.create () in
  List.iter (collect_typedef_dependencies depgraph) defs;
  depgraph

let rec substitute_named_types substmap = function
  | TypeLeaf (NamedType n) ->
      BatOption.default (TypeLeaf (NamedType n)) (StringListMap.Exceptionless.find n substmap)
  | TypeLeaf _ as t -> t
  | TypeUnion l -> TypeUnion (List.map (substitute_named_types substmap) l)
  | TypeArray t -> TypeArray (substitute_named_types substmap t)
  | TypeOption t -> TypeOption (substitute_named_types substmap t)
  | TypeNullable t -> TypeNullable (substitute_named_types substmap t)
  | TypeSequence t -> TypeSequence (substitute_named_types substmap t)

let build_subst_map defs =
  let defs =
    List.map (function (DefTypedef d, _) -> d | _ -> failwith "Non-typedef in typedef list")
      defs in                                    
  let deps = collect_dependencies defs in
  let initial_subst_map =
    List.fold_left (fun map (name, types, attrs) ->
                      if attrs <> [] then failwith "typedef with attrs not handled";
                      StringListMap.add name types map) StringListMap.empty defs
  in let module DFS = Graph.Traverse.Dfs(G) in
    begin if DFS.has_cycle deps then failwith "Cyclic typedefs" end;
    let module Top = Graph.Topological.Make(G) in
    Top.fold (fun name map ->
                if StringListMap.mem name initial_subst_map then
                  StringListMap.add name (substitute_named_types map
                                        (StringListMap.find name initial_subst_map)) map
                else map (* We may get spurios named types, since typedefs may refer to other, external named types. *))
      deps StringListMap.empty

let resolve_typedefs defs =
  let (typedefs, defs) =
    List.partition (function (DefTypedef _, _) -> true | _ -> false) defs in
  let subst_map = build_subst_map typedefs in
  let subst = substitute_named_types subst_map in
  let rec subst_args l = List.map (fun ((name, ty, mode, value), attrs) ->
                               (name, subst ty, mode, value), subst_attrs attrs) l
  and subst_attrs l = List.map (function
                                | WithArguments (name, id, args) ->
                                    WithArguments (name, id, subst_args args)
                                | WithoutArguments _ as w -> w) l
  in let subst_if_members (mem, attr) =
    ((match mem with
        | StringifierEmptyMember -> StringifierEmptyMember
        | StringifierOperationMember (name, ty, args) ->
            StringifierOperationMember (name, subst ty, subst_args args)
        | StringifierAttributeMember (name, inh, ro, ty) ->
            StringifierAttributeMember (name, inh, ro, subst ty)
        | OperationMember (name, ty, args, quals) ->
            OperationMember (name, subst ty, subst_args args, quals)
        | AttributeMember (name, inh, ro, ty) -> AttributeMember (name, inh, ro, subst ty)
        | ConstMember (name, ty, value) ->
            ConstMember (name, subst ty, value)),
     subst_attrs attr)
  and subst_ex_members (mem, attr) =
    ((match mem with
        | ExConstMember (name, ty, value) -> ExConstMember (name, subst ty, value)
        | ExValueMember (name, ty) -> ExValueMember (name, subst ty)),
     subst_attrs attr)
  and subst_di_members ((name, ty, value), attrs) =
    ((name, subst ty, value), subst_attrs attrs)
  in List.map (fun (def, attrs) ->
                 ((match def with
                 | DefDictionary (name, mode, mem) ->
                     DefDictionary (name, mode, List.map subst_di_members mem)
                 | DefEnum _ as e -> e
                 | DefTypedef _ -> failwith "Typedef encountered after filtering"
                 | DefModule _ -> failwith "Module encountered after filtering"
                 | DefImplements _ as i -> i
                 | DefInterface (name, mode, mem) ->
                     DefInterface (name, mode, List.map subst_if_members mem)
                 | DefException (name, inh, mem) ->
                     DefException (name, inh, List.map subst_ex_members mem)
                 | DefCallbackInterface (name, mode, mem) ->
                     DefCallbackInterface (name, mode, List.map subst_if_members mem)
                 | DefCallback (name, ty, args) ->
                     DefCallback (name, subst ty, subst_args args)), subst_attrs attrs))
       defs

let remove_attrs bad attrs =
  List.filter (function
                 | WithArguments (name, _, _)
                 | WithoutArguments (name, _) ->
                     if List.mem name bad then begin
                       prerr_endline("Bad argument " ^ name ^ " given on partial item");
                       false
                     end else true) attrs

let merge_partials defs =
  let interface_defs =
    List.fold_left
      (fun id -> function
         | (DefInterface (name, mode, mem), attrs) ->
             StringListMap.modify_def (ModePartial, [], []) name
               (fun (mode', mem', attrs') ->
                  match mode, mode' with
                    | ModePartial, _ ->
                        (mode', mem' @ mem,
                         remove_attrs ["ArrayClass"; "Constructor"; "ImplicitThis";
                                       "NamedConstructor"; "NoInterfaceObject"] attrs' @
                         attrs)
                    | _, ModePartial -> (mode, mem' @ mem, attrs' @ attrs)
                    | _, _ -> failwith
                                ("Multiple non-partial definitions given for interface " ^
                                 (BatString.join "::" name))) id
         | _ -> id) StringListMap.empty defs
  and dictionary_defs =
    List.fold_left
      (fun dd -> function
         | (DefDictionary (name, mode, mem), attrs) ->
             StringListMap.modify_def (ModePartial, [], []) name
               (fun (mode', mem', attrs') ->
                  match mode, mode' with
                    | ModePartial, _ ->
                        (mode', mem' @ mem, attrs' @ attrs)
                    | _, ModePartial -> (mode, mem' @ mem, attrs' @ attrs)
                    | _, _ -> failwith
                                ("Multiple non-partial definitions given for dictionary " ^
                                 (BatString.join "::" name))) dd
         | _ -> dd) StringListMap.empty defs
  and rest = List.filter
               (function (DefInterface _, _) | (DefDictionary _, _) -> false | _ -> true)
               defs
  in rest |>
       StringListMap.fold (fun name (mode, mem, attrs) defs ->
                       if mode = ModePartial then
                         failwith("No non-partial definition given for interface " ^ (BatString.join "::" name));
                       (DefInterface (name, mode, mem), attrs) :: defs) interface_defs |>
       StringListMap.fold (fun name (mode, mem, attrs) defs ->
                       if mode = ModePartial then
                         failwith("No non-partial definition given for dictionary " ^ (BatString.join "::" name));
                       (DefDictionary (name, mode, mem), attrs) :: defs) dictionary_defs

let flat_map f l = BatList.map f l |> BatList.flatten

(* Stupid naming conventions. We have to map over basically everything to set the right
 * prefixes.
 *
 * First, provide name builders to implement WebIDL's hare-brained module naming rules...
 *)
let module_name prefix name attrs =
  (* Ignore the bit about the [Prefix] attribute for now. It won't parse anyway. *)
  let qname = name :: prefix in
    if qname = ["dom"] then ["dom";"w3c";"org"]
    else if prefix = [] then [name;"dom";"w3c";"org"]
    else name :: prefix

let qname_from_identifier prefix = function
  | [name] -> name :: prefix
  | _ -> failwith "Bad name given"
let qname_from_name prefix = function
  | [name] -> name :: prefix
  | qname -> qname

let name_base_types prefix = function
  | NamedType name -> NamedType (qname_from_name prefix name)
  | other -> other
let rec name_types prefix = function
  | TypeLeaf t -> TypeLeaf (name_base_types prefix t)
  | TypeUnion ts -> TypeUnion (List.map (name_types prefix) ts)
  | TypeArray t -> TypeArray (name_types prefix t)
  | TypeOption t -> TypeOption (name_types prefix t)
  | TypeNullable t -> TypeNullable (name_types prefix t)
  | TypeSequence t -> TypeSequence (name_types prefix t)

let name_argument_data prefix (name, t, mode, value) =
  (name, name_types prefix t, mode, value)
let rec name_argument prefix (arg, attrs) =
  (name_argument_data prefix arg, name_extended_attributes prefix attrs)
and name_extended_attribute prefix = function
  | WithArguments (name, equ, args) ->
      WithArguments (name, equ, name_arguments prefix args)
  | WithoutArguments _ as other -> other
and name_arguments prefix args = List.map (name_argument prefix) args
and name_extended_attributes prefix attrs =
  List.map (name_extended_attribute prefix) attrs

let name_stringifier_operation_data prefix (name, t, args) =
  (name, name_types prefix t, name_arguments prefix args)
let name_const_data prefix (name, t, value) = (name, name_types prefix t, value)
let name_attribute_data prefix (name, f1, f2, t) = (name, f1, f2, name_types prefix t)
let name_operation_data prefix (name, rt, args, q) =
  (name, name_types prefix rt, name_arguments prefix args, q)
let name_members prefix = function
  | StringifierEmptyMember -> StringifierEmptyMember
  | StringifierOperationMember o ->
      StringifierOperationMember (name_stringifier_operation_data prefix o)
  | StringifierAttributeMember a ->
      StringifierAttributeMember (name_attribute_data prefix a)
  | OperationMember o -> OperationMember (name_operation_data prefix o)
  | AttributeMember a -> AttributeMember (name_attribute_data prefix a)
  | ConstMember c -> ConstMember (name_const_data prefix c)
let name_exception_member prefix = function
  | ExConstMember c -> ExConstMember (name_const_data prefix c)
  | ExValueMember (name, t) -> ExValueMember (name, name_types prefix t)
let name_mode prefix = function
  | ModeInherit p -> ModeInherit (qname_from_name prefix p)
  | other -> other

let name_dictionary_entry prefix (name, t, v) = (name, name_types prefix t, v)
let name_dictionary_member prefix (d, a) =
  (name_dictionary_entry prefix d, name_extended_attributes prefix a)
let name_interface_member prefix (m, a) =
  (name_members prefix m, name_extended_attributes prefix a)
let name_dictionary_data prefix (n, m, d): dictionary_data =
  (qname_from_identifier prefix n, name_mode prefix m,
   List.map (name_dictionary_member prefix) d)
let name_interface_data prefix (n, m, d) =
  (qname_from_identifier prefix n, name_mode prefix m,
   List.map (name_interface_member prefix) d)
let name_exception_data prefix (n, i, m) =
  (qname_from_identifier prefix n, BatOption.map (qname_from_name prefix) i,
   List.map (fun (e, a) ->
               (name_exception_member prefix e, name_extended_attributes prefix a)) m)
let name_enum_data prefix (n, d) = (qname_from_identifier prefix n, d)
let name_typedef_data prefix (n, t, a) =
  (qname_from_identifier prefix n, name_types prefix t, name_extended_attributes prefix a)
let name_callback_data prefix (n, t, a) =
  (qname_from_identifier prefix n, name_types prefix t, name_arguments prefix a)
let name_implements_data prefix (n1, n2) =
  (qname_from_name prefix n1, qname_from_name prefix n2)

let resolve_modules defs =
  let rec impl prefix defs =
    flat_map
      (fun (d, a) ->
         let a = name_extended_attributes prefix a in
         match d with
           | DefDictionary d -> [DefDictionary (name_dictionary_data prefix d), a]
           | DefEnum e -> [DefEnum (name_enum_data prefix e), a]
           | DefInterface d -> [DefInterface (name_interface_data prefix d), a]
           | DefException d -> [DefException (name_exception_data prefix d), a]
           | DefTypedef d -> [DefTypedef (name_typedef_data prefix d), a]
           | DefImplements d -> [DefImplements (name_implements_data prefix d), a]
           | DefCallback d -> [DefCallback (name_callback_data prefix d), a]
           | DefCallbackInterface d ->
               [DefCallbackInterface (name_interface_data prefix d), a]
           | DefModule (name, defs) -> impl (name :: prefix) defs
      ) defs
  in impl [] defs

let cleanup defs =
  defs
    |> resolve_modules
    |> resolve_typedefs
    |> merge_partials

