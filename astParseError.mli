type message_set
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
val check_and_merge_if_not_failed : ctx -> bool
val merge : ctx -> bool
val ctx_top : unit -> ctx
val ctx_push : ctx -> ctx
val pp_ctx : Format.formatter -> ctx -> unit
val warn : ctx -> ('a, Format.formatter, unit, unit) format4 -> 'a
val error : ctx -> ('a, Format.formatter, unit, unit) format4 -> 'a
val flush_errors : ctx -> unit
val flush_errors_and_handle_failure : ctx -> unit
