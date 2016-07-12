open IdlData

module Vertex = struct
  type t = Global | Class of qualified_name | Instance of qualified_name [@@deriving ord,eq]
  let hash (x: t) = Hashtbl.hash x
end
module Edge = struct
  type edgetype = Attribute of string | Result of string [@@deriving ord]
  type edgemark = Good | Nondeterministic | Blacklisted [@@deriving ord]
  type t = edgetype * edgemark [@@deriving ord]
  let default = (Attribute "(bad)", Blacklisted)
end
open Vertex
module G = Graph.Persistent.Digraph.ConcreteBidirectionalLabeled(Vertex)(Edge)

let rec fold_typenames f t x = match t with
  | NamedType t -> f x t
  | UnionType l -> List.fold_left (fun x t -> fold_typenames f t x) x l
  | ArrayType t -> fold_typenames f t x
  | OptionType (_, t) -> fold_typenames f t x
  | NullableType t -> fold_typenames f t x
  | SequenceType t -> fold_typenames f t x
  | _ -> x

let add_edge g from edgetype user_attributes typename =
  let edgemark =
    List.fold_left (fun mode -> function
                      | IdlData.UAPlain "Blacklisted"
                      | IdlData.UAPlain "ToplevelBlacklisted" ->
                          Edge.Blacklisted
                      | IdlData.UAPlain "Nondeterministic" ->
                          if mode <> Edge.Blacklisted then
                            Edge.Nondeterministic
                          else
                            mode
                      | _ -> mode)
      Edge.Good user_attributes
  in
  G.add_edge_e g (from, (edgetype, edgemark), Instance typename)

let attribute_edges name aname user_attributes types =
  fold_typenames (fun g -> add_edge g name (Edge.Attribute aname) user_attributes) types

let operation_edges name aname user_attributes types =
  fold_typenames (fun g -> add_edge g name (Edge.Result aname) user_attributes) types

let apply_map f x = BatOption.apply (BatOption.map f x)

let accessor_edges src prefix { getter; setter; creator; deleter } g =
  let add prefix ({ types; user_attributes }: attributed_type) =
    operation_edges src prefix user_attributes types
  in let add' prefix (ty, _) g = add prefix ty g
  in g |>
       apply_map (add "(getter)") getter |>
       apply_map (add "(deleter)") deleter |>
       apply_map (add' "(setter)") setter |>
       apply_map (add' "(creator)") creator

let dictionary_reachability_graph name ({ members }: dictionary) g =
  List.fold_left
    (fun g ({ name = aname; types; user_attributes }: dictionary_entry) ->
       attribute_edges (Instance name) aname user_attributes types g)
    g members

let rec last = function
  | [] -> raise Not_found
  | [x] -> x
  | _ :: l -> last l

let interface_reachability_graph
      name { consts; attributes; operations; static_operations;
             constructors; named_properties; indexed_properties;
             legacy_callers; not_exposed } g =
  let fold f l x = List.fold_left (fun a b -> f b a) x l
  and namei = Instance name and namec = Class name in
    g |>
      fold (fun ({ name; types; user_attributes }: constant) ->
              attribute_edges namec name user_attributes types) consts |>
      fold (fun ({ name; types; user_attributes }: attribute) ->
              attribute_edges namei name user_attributes types) attributes |>
      fold (fun ({ name; return; user_attributes }: operation) ->
              operation_edges namec name user_attributes return) static_operations |>
      fold (fun ({ name; return; user_attributes }: operation) ->
              operation_edges namei name user_attributes return) operations |>
      fold (fun ({ name = cname; user_attributes }: constructor) ->
              operation_edges Global (last cname) user_attributes (NamedType name)) constructors |>
      accessor_edges namei "indexed" indexed_properties |>
      accessor_edges namei "named" named_properties |>
      fold (fun ({ return; user_attributes }: legacy_caller) ->
              operation_edges namei "(legacy)" user_attributes return) legacy_callers |>
      fun g ->
        if not_exposed then
          g
        else
          add_edge g Global (Edge.Result (last name)) [] name



let build_reachability_graph defs =
  (* Ignore exception-based control flow; just check if
   * exception fields are weird. Also ignore callbacks
   * and callback interfaces - they are user-provided.
   * Enumerations don't have non-string values and are harmless.
   * This leaves dictionaries and interfaces.
  *)
  G.empty |>
    QNameMap.fold dictionary_reachability_graph defs.dictionaries |>
    QNameMap.fold interface_reachability_graph defs.interfaces |>
    fun g -> add_edge g Global (Edge.Attribute "window") [] ["Window"] |>
    fun g -> add_edge g Global (Edge.Attribute "document") [] ["Document"]


