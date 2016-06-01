open Ast
(** First step: Resolve partials and typedefs. *)                     
(* For typedef ordering *)
module Vertex = struct
  type t = string
  let equal = BatString.equal
  let compare = String.compare
  let hash: string -> int = Hashtbl.hash
end
module G = Graph.Imperative.Digraph.Concrete(Vertex)
module StringMap = BatMap.Make(BatString)

let rec iter_type_names f = function
  | TypeLeaf (NamedType n) -> f n
  | TypeLeaf _ -> ()
  | TypeUnion l -> List.iter (iter_type_names f) l
  | TypeArray t | TypeOption t | TypeNullable t | TypeSequence t -> iter_type_names f t

let collect_typedef_dependencies depgraph (name, def, _) =
  iter_type_names (fun name' -> G.add_edge depgraph name' name) def
let collect_dependencies defs =
  let depgraph = G.create () in
  List.iter (collect_typedef_dependencies depgraph) defs;
  depgraph

let rec substitute_named_types substmap = function
  | TypeLeaf (NamedType n) ->
      BatOption.default (TypeLeaf (NamedType n)) (StringMap.Exceptionless.find n substmap)
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
                      StringMap.add name types map) StringMap.empty defs
  in let module DFS = Graph.Traverse.Dfs(G) in
    begin if DFS.has_cycle deps then failwith "Cyclic typedefs" end;
    let module Top = Graph.Topological.Make(G) in
    Top.fold (fun name map ->
                StringMap.add name (substitute_named_types map
                                   (StringMap.find name initial_subst_map)) map)
      deps StringMap.empty

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
             StringMap.modify_def (ModePartial, [], []) name
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
                                 name)) id
         | _ -> id) StringMap.empty defs
  and dictionary_defs =
    List.fold_left
      (fun dd -> function
         | (DefDictionary (name, mode, mem), attrs) ->
             StringMap.modify_def (ModePartial, [], []) name
               (fun (mode', mem', attrs') ->
                  match mode, mode' with
                    | ModePartial, _ ->
                        (mode', mem' @ mem, attrs' @ attrs)
                    | _, ModePartial -> (mode, mem' @ mem, attrs' @ attrs)
                    | _, _ -> failwith
                                ("Multiple non-partial definitions given for dictionary " ^
                                 name)) dd
         | _ -> dd) StringMap.empty defs
  and rest = List.filter
               (function (DefInterface _, _) | (DefDictionary _, _) -> false | _ -> true)
               defs
  in rest |>
       StringMap.fold (fun name (mode, mem, attrs) defs ->
                       if mode = ModePartial then
                         failwith("No non-partial definition given for interface " ^ name);
                       (DefInterface (name, mode, mem), attrs) :: defs) interface_defs |>
       StringMap.fold (fun name (mode, mem, attrs) defs ->
                       if mode = ModePartial then
                         failwith("No non-partial definition given for dictionary " ^ name);
                       (DefDictionary (name, mode, mem), attrs) :: defs) dictionary_defs

let cleanup defs = merge_partials (resolve_typedefs defs)

