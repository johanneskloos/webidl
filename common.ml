type value =
  | VBool of bool
  | VInt of int
  | VFloat of float
  | VNull
  | VString of string
  [@@deriving show]

type int_length = IShort | ILong | ILongLong [@@deriving show]
type int_type = { unsigned: bool; length: int_length } [@@deriving show]
type float_type = { unrestricted: bool; double: bool } [@@deriving show]

