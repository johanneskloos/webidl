open AstParseError
type ctx = AstParseError.ctx

type 'a arg_handler = 'a -> ctx -> string option -> SimpleAst.arguments option -> 'a

let xattr_plain id (f: ctx -> 'a -> 'a): string * 'a arg_handler =
  (id, fun state ctx equ args ->
     begin match equ with
       | Some equ -> warn ctx "Unexpected `=%s' for %s" equ id
       | None -> ()
     end;
     begin match args with
       | Some _ -> warn ctx "Unexpected argument declaration for %s" id
       | None -> ()
     end;
     f ctx state)

let xattr_equals id (f: ctx -> 'a -> string -> 'a): string * 'a arg_handler =
  (id, fun state ctx equ args ->
     begin match args with
       | Some _ -> warn ctx "Unexpected argument declaration for %s" id
       | None -> ()
     end;
     begin match equ with
       | Some equ -> f ctx state equ
       | None -> error ctx "Missing `=RHS' for %s" id; state
     end)


let xattr_maybe_arguments
      id (f: ctx -> 'a -> SimpleAst.arguments option -> 'a): string * 'a arg_handler =
  (id, fun state ctx equ args ->
     begin match equ with
       | Some equ -> warn ctx "Unexpected `=%s' for %s" equ id
       | None -> ()
     end;
     f ctx state args)

let xattr_equals_maybe_arguments
      id
      (f: ctx -> 'a -> string -> SimpleAst.arguments option -> 'a): string * 'a arg_handler =
  (id, fun state ctx equ args ->
     begin match equ with
       | Some equ -> f ctx state equ args
       | None -> error ctx "Missing `=RHS' for %s" id; state
     end)

let xattr_equals_specific
      id (cases: (string * ('a -> 'a)) list): string * 'a arg_handler =
  xattr_equals
    id
    (fun ctx state key ->
       try (List.assoc key cases) state
       with Not_found ->
         error ctx "Unexpected value `%s' for %s" key id;
         state)

let handle_one state ctx handlers = let open SimpleAst in fun { name; equals; arguments } ->
  try
    (true, List.assoc name handlers state ctx equals arguments)
  with Not_found -> (false, state)

let handle_non_failing_known state ctx handlers (xattrs: SimpleAst.extended_attribute_list) =
  List.fold_left (fun (state, unhandled) xattr ->
                    let (handled, state') = handle_one state (ctx_push_state ctx) handlers xattr
                    in if handled && check_and_merge_state_if_not_failed ctx then
                      (state', unhandled)
                    else (state, xattr :: unhandled))
    (state, []) xattrs

let handle_all_known state ctx handlers xattrs =
  List.fold_left (fun (state, unhandled) xattr ->
                    let (handled, state') = handle_one state ctx handlers xattr
                    in if handled then
                      (state', unhandled)
                    else (state, xattr :: unhandled))
    (state, []) xattrs

let get_name ({ SimpleAst.name }: SimpleAst.extended_attribute) = name

let partition_attributes l =
  List.partition (fun xattr -> List.mem (get_name xattr) l)

let keep_good_attributes l xattrs = fst (partition_attributes l xattrs)
let drop_bad_attributes ctx l xattrs =
  let (good, bad) = partition_attributes l xattrs in
    if bad <> [] then begin
      warn ctx "Inadmissible extended attributes: %a" (Fmt.list Fmt.string)
        (List.map get_name bad)
    end;
    good
