(** blocking.proto Generated Types and Encoding *)


(** {2 Types} *)

type path = {
  attribute : string;
  path : string list;
}

type blocking = {
  blacklisted : path list;
  nondeterministic : path list;
}


(** {2 Default values} *)

val default_path : 
  ?attribute:string ->
  ?path:string list ->
  unit ->
  path
(** [default_path ()] is the default value for type [path] *)

val default_blocking : 
  ?blacklisted:path list ->
  ?nondeterministic:path list ->
  unit ->
  blocking
(** [default_blocking ()] is the default value for type [blocking] *)


(** {2 Protobuf Decoding} *)

val decode_path : Pbrt.Decoder.t -> path
(** [decode_path decoder] decodes a [path] value from [decoder] *)

val decode_blocking : Pbrt.Decoder.t -> blocking
(** [decode_blocking decoder] decodes a [blocking] value from [decoder] *)


(** {2 Protobuf Toding} *)

val encode_path : path -> Pbrt.Encoder.t -> unit
(** [encode_path v encoder] encodes [v] with the given [encoder] *)

val encode_blocking : blocking -> Pbrt.Encoder.t -> unit
(** [encode_blocking v encoder] encodes [v] with the given [encoder] *)


(** {2 Formatters} *)

val pp_path : Format.formatter -> path -> unit 
(** [pp_path v] formats v *)

val pp_blocking : Format.formatter -> blocking -> unit 
(** [pp_blocking v] formats v *)
