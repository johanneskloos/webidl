let pp_position = let open Lexing in
  fun fmt { pos_fname; pos_lnum; pos_bol; pos_cnum } ->
    Format.fprintf fmt "(%s:%d:%d)" pos_fname pos_lnum (pos_cnum - pos_bol)

