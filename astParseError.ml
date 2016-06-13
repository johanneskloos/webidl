type message = Warning of string | Error of string
type message_set = {
  mutable messages: message list;
  mutable failed: bool
}
type ctx =
    CtxTop of message_set
  | CtxArg of string * ctx
  | CtxConst of string * ctx
  | CtxAttr of string * ctx
  | CtxRet of ctx
  | CtxOp of string * ctx
  | CtxOpLegacy of string option * ctx
  | CtxSpecial of string option * ctx
  | CtxStringifierAttribute of string * ctx
  | CtxStringifier of ctx
  | CtxStringifierOperation of string option * ctx
  | CtxConstructor of string * ctx
  | CtxDictionary of string * ctx
  | CtxException of string * ctx
  | CtxInterface of string * ctx
  | CtxEnumeration of string * ctx
  | CtxCallback of string option * ctx
  | CtxEntry of string * ctx
  | CtxPushState of ctx * message_set

let rec find_message_set_and_ctx = function
  | CtxTop ms -> (None, ms)
  | CtxPushState (ctx, ms) -> (Some ctx, ms)
  | CtxArg (_, ctx) | CtxConst (_, ctx) | CtxAttr (_, ctx)
  | CtxRet ctx | CtxOp (_, ctx) | CtxOpLegacy (_, ctx)
  | CtxSpecial (_, ctx) | CtxStringifierAttribute (_, ctx)
  | CtxStringifier ctx | CtxStringifierOperation (_, ctx)
  | CtxConstructor (_, ctx) | CtxDictionary (_, ctx)
  | CtxInterface (_, ctx) | CtxEnumeration (_, ctx)
  | CtxException (_, ctx) | CtxCallback (_, ctx) | CtxEntry (_, ctx) ->
      find_message_set_and_ctx ctx

let check_and_merge_if_not_failed ctx =
  match find_message_set_and_ctx ctx with
    | (Some ctx', lower) ->
        let (_, higher) = find_message_set_and_ctx ctx'
        in if lower.failed then
          false
        else begin
          higher.messages <- lower.messages @ higher.messages;
          true
        end
    | (None, only) ->
        begin
          only.messages <-
          Warning "check_and_merge_if_not_failed: nothing to merge" ::
          only.messages;
          true
        end

let merge ctx =
  match find_message_set_and_ctx ctx with
    | (Some ctx', lower) ->
        let (_, higher) = find_message_set_and_ctx ctx' in
          higher.messages <- lower.messages @ higher.messages;
          higher.failed <- lower.failed || higher.failed;
          true
    | (None, only) ->
        only.messages <-
        Warning "check_and_merge_if_not_failed: nothing to merge" ::
        only.messages;
        true
           
let ctx_top () = CtxTop { messages = []; failed = false }
let ctx_push ctx = CtxPushState (ctx, { messages = []; failed = false })

let rec pp_ctx pp = function
  | CtxPushState (ctx, _) -> pp_ctx pp ctx
  | CtxTop _ ->
    Format.fprintf pp "(toplevel)"
  | CtxArg (name, CtxTop _) ->
    Format.fprintf pp "argument %s" name
  | CtxArg (name, ctx) ->
    Format.fprintf pp "%a, argument %s" pp_ctx ctx name
  | CtxConst (name, CtxTop _) ->
    Format.fprintf pp "constant %s" name
  | CtxConst (name, ctx) ->
    Format.fprintf pp "%a, constant %s" pp_ctx ctx name
  | CtxAttr (name, CtxTop _) ->
    Format.fprintf pp "attribute %s" name
  | CtxAttr (name, ctx) ->
    Format.fprintf pp "%a, attribute %s" pp_ctx ctx name
  | CtxRet (CtxTop _) ->
    Format.fprintf pp "top-level return type"
  | CtxRet ctx ->
    Format.fprintf pp "%a, return type" pp_ctx ctx
  | CtxOp (name, CtxTop _) ->
    Format.fprintf pp "operation %s" name
  | CtxOp (name, ctx) ->
    Format.fprintf pp "%a, operation %s" pp_ctx ctx name
  | CtxOpLegacy (Some name, CtxTop _) ->
    Format.fprintf pp "legacy operation %s" name
  | CtxOpLegacy (Some name, ctx) ->
    Format.fprintf pp "%a, legacy operation %s" pp_ctx ctx name
  | CtxOpLegacy (None, CtxTop _) ->
    Format.fprintf pp "unnamed legacy operation"
  | CtxOpLegacy (None, ctx) ->
    Format.fprintf pp "%a, unnamed legacy operation" pp_ctx ctx
  | CtxSpecial (Some name, CtxTop _) ->
    Format.fprintf pp "special operation %s" name
  | CtxSpecial (Some name, ctx) ->
    Format.fprintf pp "%a, special operation %s" pp_ctx ctx name
  | CtxSpecial (None, CtxTop _) ->
    Format.fprintf pp "unnamed special operation"
  | CtxSpecial (None, ctx) ->
    Format.fprintf pp "%a, unnamed special operation" pp_ctx ctx
  | CtxStringifierAttribute (name, CtxTop _) ->
    Format.fprintf pp "attribute stringifier %s" name
  | CtxStringifierAttribute (name, ctx) ->
    Format.fprintf pp "%a, attribute stringifier %s" pp_ctx ctx name
  | CtxStringifier (CtxTop _) ->
    Format.fprintf pp "top-level stringifier"
  | CtxStringifier ctx ->
    Format.fprintf pp "%a, strinigifier" pp_ctx ctx
  | CtxStringifierOperation (Some name, CtxTop _) ->
    Format.fprintf pp "strinigifier operation %s" name
  | CtxStringifierOperation (Some name, ctx) ->
    Format.fprintf pp "%a, stringifier operation %s" pp_ctx ctx name
  | CtxStringifierOperation (None, CtxTop _) ->
    Format.fprintf pp "unnamed stringifier operation"
  | CtxStringifierOperation (None, ctx) ->
    Format.fprintf pp "%a, unnamed stringifier operation" pp_ctx ctx
  | CtxConstructor (name, CtxTop _) ->
    Format.fprintf pp "constructor %s" name
  | CtxConstructor (name, ctx) ->
    Format.fprintf pp "%a, constructor %s" pp_ctx ctx name
  | CtxDictionary (name, CtxTop _) ->
    Format.fprintf pp "dictionary %s" name
  | CtxDictionary (name, ctx) ->
    Format.fprintf pp "%a, dictionary %s" pp_ctx ctx name
  | CtxException (name, CtxTop _) ->
    Format.fprintf pp "exception %s" name
  | CtxException (name, ctx) ->
    Format.fprintf pp "%a, exception %s" pp_ctx ctx name
  | CtxInterface (name, CtxTop _) ->
    Format.fprintf pp "interface %s" name
  | CtxInterface (name, ctx) ->
    Format.fprintf pp "%a, interface %s" pp_ctx ctx name
  | CtxEnumeration (name, CtxTop _) ->
    Format.fprintf pp "enumeration %s" name
  | CtxEnumeration (name, ctx) ->
    Format.fprintf pp "%a, enumeration %s" pp_ctx ctx name
  | CtxEntry (name, CtxTop _) ->
    Format.fprintf pp "entry %s" name
  | CtxEntry (name, ctx) ->
    Format.fprintf pp "%a, entry %s" pp_ctx ctx name
  | CtxCallback (Some name, CtxTop _) ->
    Format.fprintf pp "callback %s" name
  | CtxCallback (Some name, ctx) ->
    Format.fprintf pp "%a, callback %s" pp_ctx ctx name
  | CtxCallback (None, CtxTop _) ->
    Format.fprintf pp "unnamed callback"
  | CtxCallback (None, ctx) ->
    Format.fprintf pp "%a, unnamed callback" pp_ctx ctx

let update_with_location ctx = function
  | Warning msg -> Warning (Format.asprintf "%a: %s" pp_ctx ctx msg)
  | Error msg -> Error (Format.asprintf "%a: %s" pp_ctx ctx msg)

let update_ctx_error_state ctx newmsg failed =
  let (_, ms) = find_message_set_and_ctx ctx in
    ms.messages <- newmsg :: ms.messages;
    ms.failed <- failed || ms.failed

let warn ctx fmt =
  Format.kasprintf (fun msg -> update_ctx_error_state ctx (Warning msg) false) fmt
let error ctx fmt =
  Format.kasprintf (fun msg -> update_ctx_error_state ctx (Error msg) true) fmt

let flush_errors ctx =
  let (_, ms) = find_message_set_and_ctx ctx in
    List.iter (function
                 | Warning msg -> Format.eprintf "Warning: %s" msg
                 | Error msg -> Format.eprintf "ERROR: %s" msg)
      (List.rev ms.messages);
    ms.messages <- []

let flush_errors_and_handle_failure ctx =
  flush_errors ctx;
  match find_message_set_and_ctx ctx with
    | (None, { failed = true }) ->
        Format.eprintf "Exiting due to failures";
        exit 1
    | _ -> ()


