open IdlData

module Vertex = struct
  type t = Global | Class of qualified_name | Instance of qualified_name [@@deriving ord,eq]
  let hash (x: t) = Hashtbl.hash x
end
module Edge = struct
  type t = Attribute | Result [@@deriving ord]
  let default = Attribute
end
open Vertex
module G = Graph.Imperative.Digraph.ConcreteBidirectionalLabeled(Vertex)(Edge)

let rec extract_typenames = function
  | NamedType t -> [t]
  | UnionType l -> List.flatten (List.map extract_typenames l)
  | ArrayType t -> extract_typenames t
  | OptionType (_, t) -> extract_typenames t
  | NullableType t -> extract_typenames t
  | SequenceType t -> extract_typenames t
  | _ -> []
let attribute_edges name g types =
  List.iter (fun typename -> G.add_edge_e g (name, Edge.Attribute, Instance typename))
    (extract_typenames types)
let operation_edges name g types =
  List.iter (fun typename -> G.add_edge_e g (name, Edge.Result, Instance typename))
    (extract_typenames types)

let accessor_edges name g { getter; setter; creator; deleter } =
  BatOption.may (function ({ types }: attributed_type) -> operation_edges name g types)
    getter;
  BatOption.may (function ({ types }: attributed_type) -> operation_edges name g types)
    deleter;
  BatOption.may (function (({ types }: attributed_type), _) -> operation_edges name g types)
    setter;
  BatOption.may (function (({ types }: attributed_type), _) -> operation_edges name g types)
    creator

let build_reachability_graph defs =
  (* Ignore exception-based control flow; just check if
   * exception fields are weird. Also ignore callbacks
   * and callback interfaces - they are user-provided.
   * Enumerations don't have non-string values and are harmless.
   * This leaves dictionaries and interfaces.
  *)
  let g = G.create () in
  QNameMap.iter (fun name ({ members }: dictionary) ->
      List.iter (fun ({ types }: dictionary_entry) ->
          attribute_edges (Instance name) g types)
        members)
    defs.dictionaries;
  QNameMap.iter
    (fun name { consts; attributes; operations; static_operations;
                constructors; named_properties; indexed_properties;
                legacy_callers; not_exposed } ->
       let namei = Instance name and namec = Class name in
       List.iter (fun ({ types }: constant) -> attribute_edges namec g types)
         consts;
       List.iter (fun ({ types }: attribute) -> attribute_edges namei g types)
         attributes;
       List.iter (fun ({ return }: operation) -> operation_edges namei g return)
         operations;
       List.iter (fun ({ return }: operation) -> operation_edges namec g return)
         static_operations;
       List.iter (fun _ -> G.add_edge_e g (Global, Edge.Attribute, namei))
         constructors;
       accessor_edges namei g named_properties;
       accessor_edges namei g indexed_properties;
       List.iter (fun ({ return }: legacy_caller) -> operation_edges namei g return)
         legacy_callers;
       if not not_exposed then G.add_edge g Global namec
    )
    defs.interfaces;
  G.add_edge_e g (Global, Edge.Attribute, Instance ["window"]);
  G.add_edge_e g (Global, Edge.Attribute, Instance ["document"]);
  g

let calculate_class_members p defs =
  let pred = List.exists p
  in let listpred getattr l = List.exists (fun x -> pred (getattr x)) l
  and attributed_type_pred_1: attributed_type option -> bool = function
    | Some { user_attributes } -> pred user_attributes
    | None -> false
  and attributed_type_pred_2:
                (attributed_type * attributed_type) option -> bool = function
    | Some (_, { user_attributes }) -> pred user_attributes
    | None -> false
  in let accessor_pred { getter; setter; creator; deleter } =
    attributed_type_pred_1 getter
      || attributed_type_pred_2 setter
      || attributed_type_pred_2 creator
      || attributed_type_pred_1 deleter
  in QNameMap.fold
       (fun name ({ members }: dictionary) class_members ->
          if listpred (fun (x: dictionary_entry) -> x.user_attributes) members then
            Vertex.Instance name :: class_members
          else
            class_members)
       defs.dictionaries []
    |> QNameMap.fold
       (fun name ({ consts; attributes; operations; static_operations;
                    constructors; named_properties; indexed_properties;
                    legacy_callers }) class_members ->
          let add_instance =
            listpred (fun (x: constant) -> x.user_attributes) consts
            || listpred (fun (x: attribute) -> x.user_attributes) attributes
            || listpred (fun (x: operation) -> x.user_attributes) operations
            || accessor_pred named_properties
            || accessor_pred indexed_properties
            || listpred (fun (x: legacy_caller) -> x.user_attributes) legacy_callers
          and add_class =
            listpred (fun (x: operation) -> x.user_attributes) static_operations
              || listpred (fun (x: constructor) -> x.user_attributes) constructors
          in match add_instance, add_class with
            | true, true ->
                Vertex.Instance name :: Vertex.Class name :: class_members
            | true, false ->
                Vertex.Instance name :: class_members
            | false, true ->
                Vertex.Class name :: class_members
            | false, false -> class_members)
       defs.interfaces

let calculate_blacklisted =
  calculate_class_members (function IdlData.UAPlain "Blacklisted" -> true | _ -> false)

let calculate_nondeterministic =
  calculate_class_members (function IdlData.UAPlain "Nondeterministic" -> true | _ -> false)

module AccessibleAnalysis = struct
  type edge = G.edge
  type vertex = G.vertex
  type data = bool
  type g = G.t
  let direction = Graph.Fixpoint.Forward
  let join = (||)
  let equal: data -> data -> bool = (=)
  let analyze (_, mode, _) data = if mode = Edge.Attribute then data else false
end
module Accessible = Graph.Fixpoint.Make(G)(AccessibleAnalysis)

let calculate_accessibles (g: G.t): Vertex.t -> bool =
  Accessible.analyze (fun v -> v = Vertex.Global) g

module MarkingAnalysis = struct
  type edge = G.edge
  type vertex = G.vertex
  type mode = Neither | Nondeterministic | Blacklisted
  type data = mode * bool
  type g = G.t

  let direction = Graph.Fixpoint.Backward
  let equal: data -> data -> bool = (=)

  let join_mode m1 m2 = match m1, m2 with
    | Neither, m -> m
    | m, Neither -> m
    | Nondeterministic, Nondeterministic -> Nondeterministic
    | Blacklisted, _ -> Blacklisted
    | _, Blacklisted -> Blacklisted
  let join (m1, r1) (m2, r2) = (join_mode m1 m2, r1 || r2)

  let analyze (_, _, tgt) (m, r) =
    if r then (Neither, true) else (m, false)
end
module Marking = Graph.Fixpoint.Make(G)(MarkingAnalysis)

let calculate_marking (g: G.t) blacklisted nonderministic:
      Vertex.t -> MarkingAnalysis.data =
  let acc = calculate_accessibles g
  and mode v =
    if List.mem v blacklisted then
      MarkingAnalysis.Blacklisted
    else if List.mem v nonderministic then
      MarkingAnalysis.Nondeterministic
    else
      MarkingAnalysis.Neither
  in Marking.analyze (fun v -> (mode v, acc v)) g


let mark defs =
  let blacklisted = calculate_blacklisted defs
  and nondeterministic = calculate_nondeterministic defs
  and g = build_reachability_graph defs
  in let marking = calculate_marking g blacklisted nondeterministic
  in G.fold_vertex (fun v res -> (v, marking v) :: res) g []
