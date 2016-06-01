open IdlData
(** Step 3: Flatten the inheritance and implmements hierarchy *)
module Vertex = struct
  type vtype = Interface | Dictionary | Exception [@@deriving ord, eq]
  let vtype_hash = function Interface -> 0 | Dictionary -> 1 | Exception -> 2
  type t = string * vtype [@@deriving ord, eq]
  let hash ((str, vt): t) = 3 * Hashtbl.hash str + vtype_hash vt
end

module G = Graph.Imperative.Digraph.Concrete(Vertex)
module DFS = Graph.Traverse.Dfs(G)
module Top = Graph.Topological.Make(G)

let extract_inheritance get_inh =
  StringMap.fold (fun name data inheritance -> match get_inh data with
                    | Some inh -> (name, inh) :: inheritance
                    | None -> inheritance)

let extract_interface_inheritance { interfaces; callback_interfaces; implements } =
  let extract_inh =
    extract_inheritance (function
                           | { inheritance_mode = InheritsFrom inh } -> Some inh
                           | _ -> None)
  in implements |> extract_inh interfaces |> extract_inh callback_interfaces

let build_dependencies_and_check_consistency defs =
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
    StringMap.iter (fun name _ ->
                      if StringMap.mem name defs.interfaces then
                        failwith (name ^ " defined twice, both callback and non-callback"))
      defs.callback_interfaces;
    List.iter (fun (name, inh) ->
                 if StringMap.mem name defs.callback_interfaces then begin
                   if StringMap.mem inh defs.interfaces then
                     failwith ("Callback interface " ^ name ^
                               " inherits from non-callback interface " ^ inh ^ "!")
                   else if not (StringMap.mem inh defs.callback_interfaces) then
                     failwith ("Callback interface " ^ name ^
                               " inherits from non-existant interface " ^ inh ^ "!")
                 end else if StringMap.mem name defs.interfaces then begin
                   if not (StringMap.mem inh defs.interfaces ||
                           StringMap.mem inh defs.callback_interfaces) then
                     failwith ("Interface " ^ name ^
                               " inherits from non-existant interface " ^ inh ^ "!")
                 end else
                   failwith ("Interface " ^ name ^
                             " mentioned in an implements clause does not exist!"))
      ifinh;
    List.iter (fun (name, inh) -> if not (StringMap.mem inh defs.exceptions) then
                 failwith ("Exception " ^ name ^ " inherits from non-existent exception " ^
                           inh ^ "!")) exinh;
    List.iter (fun (name, inh) -> if not (StringMap.mem inh defs.dictionaries) then
                 failwith ("Dictionary " ^ name ^ " inherits from non-existent dictionary " ^
                           inh ^ "!")) diinh;
    begin if (DFS.has_cycle dep) then
      failwith "Cyclic inheritance graph!"
    end;
    dep


let merge_dictionary ({ members = parent_members }: dictionary)
      ({ name; inherits_from; members }: dictionary) =
  { name; inherits_from; members = parent_members @ members }

let merge_exception { consts = parent_consts; members = parent_members }
      { name; inherits_from; consts; members; not_exposed } =
  { name; inherits_from; not_exposed;
    consts = parent_consts @ consts; members = parent_members @ members }

let merge_interface
      { consts = parent_consts; attributes = parent_attributes;
        operations = parent_operations; static_operations = parent_static_operations;
        constructors = parent_constructors; named_properties = parent_named_properties;
        indexed_properties = parent_indexed_properties;
        legacy_callers = parent_legacy_callers; stringifier = parent_stringifier }
      { inheritance_mode; name; consts; attributes; operations; static_operations;
        constructors; special; named_properties; indexed_properties; legacy_callers;
        not_exposed; stringifier
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
    { inheritance_mode; name; not_exposed; special;
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
  let deps = build_dependencies_and_check_consistency defs in
    Top.fold
      (fun (parent, parent_type) defs ->
         match parent_type with
           | Interface ->
               let data =
                 match StringMap.Exceptionless.find parent defs.interfaces with
                   | Some data -> data
                   | None -> StringMap.find parent defs.callback_interfaces
               in let merge = merge_interface data
               in G.fold_succ (fun (child, child_type) defs ->
                              if child_type <> Interface then
                                failwith "Inconsistent dependency edge";
                              if StringMap.mem child defs.interfaces then
                                { defs with interfaces =
                                    StringMap.modify child merge defs.interfaces }
                              else
                                { defs with callback_interfaces =
                                    StringMap.modify child merge defs.callback_interfaces })
                 deps (parent, parent_type) defs
           | Exception ->
               let merge = merge_exception (StringMap.find parent defs.exceptions)
               in G.fold_succ (fun (child, child_type) defs ->
                                 if child_type <> Exception then
                                   failwith "Inconsistent dependency edge";
                                 { defs with exceptions =
                                     StringMap.modify child merge defs.exceptions })
                    deps (parent, parent_type) defs
           | Dictionary ->
               let merge = merge_dictionary (StringMap.find parent defs.dictionaries)
               in G.fold_succ (fun (child, child_type) defs ->
                                 if child_type <> Dictionary then
                                   failwith "Inconsistent dependency edge";
                                 { defs with dictionaries =
                                     StringMap.modify child merge defs.dictionaries })
                    deps (parent, parent_type) defs)
      deps defs

