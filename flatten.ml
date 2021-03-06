open IdlData
(** Step 3: Flatten the inheritance and implmements hierarchy *)
module Vertex = struct
  type vtype = Interface | Dictionary | Exception [@@deriving ord, eq, show]
  let vtype_hash = function Interface -> 0 | Dictionary -> 1 | Exception -> 2
  type t = qualified_name * vtype [@@deriving ord, eq]
  let hash ((str, vt): t) = 3 * Hashtbl.hash str + vtype_hash vt
end

module G = Graph.Imperative.Digraph.Concrete(Vertex)
module DFS = Graph.Traverse.Dfs(G)
module Top = Graph.Topological.Make(G)

let extract_inheritance get_inh =
  QNameMap.fold (fun name data inheritance ->
                   List.map (fun inh -> (name, inh)) (get_inh data) @ inheritance)

let extract_interface_inheritance { interfaces; callback_interfaces; implements } =
  let extract_inh: IdlData.interface IdlData.QNameMap.t -> _ =
    extract_inheritance (fun (i: IdlData.interface) -> i.inheritance)
  in implements |> extract_inh interfaces |> extract_inh callback_interfaces

let build_dependencies_and_check_consistency ctx defs =
  let open Vertex in
  let dep = G.create () in
  let ifinh = extract_interface_inheritance defs
  and diinh = extract_inheritance (function ({ inherits_from }: dictionary) -> inherits_from)
                defs.dictionaries []
  and exinh = extract_inheritance (function ({ inherits_from }: exception_) -> inherits_from)
                defs.exceptions []
  and add_edges key es =
    List.iter (fun (name, inh) -> G.add_edge dep (inh, key) (name, key)) es
  in
    add_edges Interface ifinh;
    add_edges Dictionary diinh;
    add_edges Exception exinh;
    (* Consistency checks *)
    QNameMap.iter (fun name _ ->
                      if QNameMap.mem name defs.interfaces then
                        ContextError.error ctx "%a defined twice, both callback and non-callback"
                          IdlData.pp_qualified_name name)
      defs.callback_interfaces;
    List.iter (fun (name, inh) ->
                 if QNameMap.mem name defs.callback_interfaces then begin
                   if QNameMap.mem inh defs.interfaces then
                        ContextError.error ctx "Callback interface %a inherits from non-callback interface %a"
                          IdlData.pp_qualified_name name IdlData.pp_qualified_name inh
                   else if not (QNameMap.mem inh defs.callback_interfaces) then
                        ContextError.error ctx "Callback interface %a inherits from non-existent interface %a"
                          IdlData.pp_qualified_name name IdlData.pp_qualified_name inh
                 end else if QNameMap.mem name defs.interfaces then begin
                   if not (QNameMap.mem inh defs.interfaces ||
                           QNameMap.mem inh defs.callback_interfaces) then
                        ContextError.error ctx "Interface %a inherits from non-existent interface %a"
                          IdlData.pp_qualified_name name IdlData.pp_qualified_name inh
                 end else
                   ContextError.error ctx "Interface %a on the LHS of an implements clause does not exist!"
                     IdlData.pp_qualified_name name)
      ifinh;
    List.iter (fun (name, inh) -> if not (QNameMap.mem inh defs.exceptions) then
                 ContextError.error ctx "Exception %a inherits from non-existent exception %a"
                   IdlData.pp_qualified_name name IdlData.pp_qualified_name inh)
      exinh;
    List.iter (fun (name, inh) -> if not (QNameMap.mem inh defs.dictionaries) then
                 ContextError.error ctx "Dictionary %a inherits from non-existent dictionary %a"
                   IdlData.pp_qualified_name name IdlData.pp_qualified_name inh)
      diinh;
    begin if (DFS.has_cycle dep) then
      ContextError.error ctx "Cyclic inheritance graph!"
    end;
    ContextError.flush_errors_and_handle_failure ctx;
    dep


let merge_dictionary ({ members = parent_members }: dictionary)
      ({ name; inherits_from; members; user_attributes }: dictionary) =
  { name; inherits_from; user_attributes; members = parent_members @ members }

let merge_exception { consts = parent_consts; members = parent_members }
      { name; inherits_from; consts; members; not_exposed; user_attributes } =
  { name; inherits_from; not_exposed; user_attributes;
    consts = parent_consts @ consts; members = parent_members @ members }

let merge_interface
      { consts = parent_consts; attributes = parent_attributes;
        operations = parent_operations; static_operations = parent_static_operations;
        constructors = parent_constructors; named_properties = parent_named_properties;
        indexed_properties = parent_indexed_properties;
        legacy_callers = parent_legacy_callers; stringifier = parent_stringifier }
      { inheritance; name; consts; attributes; operations; static_operations;
        constructors; special; named_properties; indexed_properties; legacy_callers;
        not_exposed; stringifier; user_attributes
      } = 
  let merge_properties
        { getter = parent_getter; setter = parent_setter;
          deleter = parent_deleter; creator = parent_creator }
        { getter; setter; deleter; creator } =
    let def = function Some x -> (fun _ -> Some x) | None -> (fun x -> x) in
      { getter = def getter parent_getter;
        setter = def setter parent_setter; 
        deleter = def deleter parent_deleter;
        creator = def creator parent_creator }
  in
    { inheritance; name; not_exposed; special; user_attributes;
      consts = parent_consts @ consts;
      attributes = parent_attributes @ attributes;
      operations = parent_operations @ operations;
      static_operations = parent_static_operations @ static_operations;
      constructors = parent_constructors @ constructors;
      named_properties = merge_properties parent_named_properties named_properties;
      indexed_properties = merge_properties parent_indexed_properties indexed_properties;
      legacy_callers = parent_legacy_callers @ legacy_callers;
      stringifier = if stringifier = NoStringifier then parent_stringifier else stringifier }

let flatten defs =
  let open Vertex in
  let ctx = ContextError.ctx_top () in
  let deps = build_dependencies_and_check_consistency ctx defs in
  let flat =
    Top.fold
      (fun (parent, parent_type) defs ->
         match parent_type with
           | Interface ->
               let data =
                 match QNameMap.Exceptionless.find parent defs.interfaces with
                   | Some data -> data
                   | None -> QNameMap.find parent defs.callback_interfaces
               in let merge = merge_interface data
               in G.fold_succ (fun (child, child_type) defs ->
                              if child_type <> Interface then
                                ContextError.error ctx
                                  "Inconsistent dependency edge from %a to %a (interface to %a)"
                                  IdlData.pp_qualified_name parent
                                  IdlData.pp_qualified_name child
                                  Vertex.pp_vtype child_type;
                              if QNameMap.mem child defs.interfaces then
                                { defs with interfaces =
                                    QNameMap.modify child merge defs.interfaces }
                              else
                                { defs with callback_interfaces =
                                    QNameMap.modify child merge defs.callback_interfaces })
                 deps (parent, parent_type) defs
           | Exception ->
               let merge = merge_exception (QNameMap.find parent defs.exceptions)
               in G.fold_succ (fun (child, child_type) defs ->
                                 if child_type <> Exception then
                                   ContextError.error ctx
                                     "Inconsistent dependency edge from %a to %a (exception to %a)"
                                     IdlData.pp_qualified_name parent
                                     IdlData.pp_qualified_name child
                                     Vertex.pp_vtype child_type;
                                 { defs with exceptions =
                                     QNameMap.modify child merge defs.exceptions })
                    deps (parent, parent_type) defs
           | Dictionary ->
               let merge = merge_dictionary (QNameMap.find parent defs.dictionaries)
               in G.fold_succ (fun (child, child_type) defs ->
                                 if child_type <> Dictionary then
                                   ContextError.error ctx
                                     "Inconsistent dependency edge from %a to %a (dictionary to %a)"
                                     IdlData.pp_qualified_name parent
                                     IdlData.pp_qualified_name child
                                     Vertex.pp_vtype child_type;
                                 { defs with dictionaries =
                                     QNameMap.modify child merge defs.dictionaries })
                    deps (parent, parent_type) defs)
      deps defs
  in ContextError.flush_errors_and_handle_failure ctx; flat
