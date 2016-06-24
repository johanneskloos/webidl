type ctx
val check_and_merge_state_if_not_failed : ctx -> bool
val merge_state : ctx -> bool
val ctx_top : unit -> ctx
val ctx_push_state : ctx -> ctx
val ctx_push_scope : ctx -> ('a, Format.formatter, unit, ctx) format4 -> 'a
val pp_ctx : Format.formatter -> ctx -> unit
val warn : ctx -> ('a, Format.formatter, unit, unit) format4 -> 'a
val error : ctx -> ('a, Format.formatter, unit, unit) format4 -> 'a
val flush_errors : ctx -> unit
val flush_errors_and_handle_failure : ctx -> unit
