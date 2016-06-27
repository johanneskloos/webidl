type message = Warning of string | Error of string
type state = {
  mutable message_list: (message * string list) list;
  mutable failed: bool
}
type ctx = {
  saved_state_stack: state list;
  current_state: state;
  context_stack: string list
}

let add_message ctx msg =
  ctx.current_state.message_list <-
    (msg, ctx.context_stack) :: ctx.current_state.message_list;
  begin match msg with
      Error _ -> ctx.current_state.failed <- true
    | Warning _ -> ()
  end

let warn ctx fmt = Format.kasprintf (fun s -> add_message ctx (Warning s)) fmt
let error ctx fmt = Format.kasprintf (fun s -> add_message ctx (Error s)) fmt

let make_state () = { message_list = []; failed = false }
let ctx_top () =
  { saved_state_stack = []; current_state = make_state(); context_stack = [] }
let ctx_push_state ctx =
  { saved_state_stack = ctx.current_state :: ctx.saved_state_stack;
    current_state = make_state();
    context_stack = ctx.context_stack }
let ctx_push_scope ({ context_stack } as ctx) fmt =
  Format.kasprintf (fun s -> { ctx with context_stack = s :: context_stack }) fmt

let merge_state ctx =
  (* Use stateful features here. This code is entirely imperative! *)
  match ctx.saved_state_stack with
    | merge_state :: _->
        merge_state.message_list <- ctx.current_state.message_list @ merge_state.message_list;
        merge_state.failed <- ctx.current_state.failed || merge_state.failed;
        ctx.current_state.failed
    | [] ->
        error ctx "Trying to merge states when no saved state exists!";
        true

let check_and_merge_state_if_not_failed ctx =
  if not (ctx.current_state.failed) then
    let _ = merge_state ctx in false
  else
    true

let pp_ctx pp ctx =
  let open Fmt in
  let pp_context pp = function
    | [] -> string pp "the top"
    | _ as l -> list ~sep:(const string ", ") string pp l
  in let pp_message pp (msg, ctx) = match msg with
    | Warning msg ->
        Format.fprintf pp "@[<hov 2>Warning: At @[<h>%a@], %s.@]@ "
          pp_context ctx msg
    | Error msg ->
        Format.fprintf pp "@[<hov 2>ERROR: At @[<h>%a@], %s!@]@ "
          pp_context ctx msg
  in vbox (list ~sep:cut pp_message) pp ctx.current_state.message_list

let flush_errors ctx =
  pp_ctx Format.err_formatter ctx;
  ctx.current_state.message_list <- []

let flush_errors_and_handle_failure ctx =
  flush_errors ctx;
  if ctx.current_state.failed then begin
    Format.eprintf "Exiting due to errors@.";
    exit 1
  end

