type value =
  | VBool of bool
  | VInt of int
  | VFloat of float
  | VNull
  | VString of string
let pp_value pp = function
  | VBool b -> Fmt.bool pp b
  | VInt i -> Fmt.int pp i
  | VFloat f -> Fmt.float pp f
  | VString s -> Fmt.string pp s
  | VNull -> Fmt.string pp "null"

type int_length = IShort | ILong | ILongLong [@@deriving show]
type int_type = { unsigned: bool; length: int_length } [@@deriving show]
type float_type = { unrestricted: bool; double: bool } [@@deriving show]

