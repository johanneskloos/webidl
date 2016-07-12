[@@@ocaml.warning "-27-30-39"]

type path = {
  attribute : string;
  path : string list;
}

and path_mutable = {
  mutable attribute : string;
  mutable path : string list;
}

type blocking = {
  blacklisted : path list;
  nondeterministic : path list;
}

and blocking_mutable = {
  mutable blacklisted : path list;
  mutable nondeterministic : path list;
}

let rec default_path 
  ?attribute:((attribute:string) = "")
  ?path:((path:string list) = [])
  () : path  = {
  attribute;
  path;
}

and default_path_mutable () : path_mutable = {
  attribute = "";
  path = [];
}

let rec default_blocking 
  ?blacklisted:((blacklisted:path list) = [])
  ?nondeterministic:((nondeterministic:path list) = [])
  () : blocking  = {
  blacklisted;
  nondeterministic;
}

and default_blocking_mutable () : blocking_mutable = {
  blacklisted = [];
  nondeterministic = [];
}

let rec decode_path d =
  let v = default_path_mutable () in
  let rec loop () = 
    match Pbrt.Decoder.key d with
    | None -> (
      v.path <- List.rev v.path;
    )
    | Some (1, Pbrt.Bytes) -> (
      v.attribute <- Pbrt.Decoder.string d;
      loop ()
    )
    | Some (1, pk) -> raise (
      Protobuf.Decoder.Failure (Protobuf.Decoder.Unexpected_payload ("Message(path), field(1)", pk))
    )
    | Some (2, Pbrt.Bytes) -> (
      v.path <- (Pbrt.Decoder.string d) :: v.path;
      loop ()
    )
    | Some (2, pk) -> raise (
      Protobuf.Decoder.Failure (Protobuf.Decoder.Unexpected_payload ("Message(path), field(2)", pk))
    )
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind; loop ()
  in
  loop ();
  let v:path = Obj.magic v in
  v

let rec decode_blocking d =
  let v = default_blocking_mutable () in
  let rec loop () = 
    match Pbrt.Decoder.key d with
    | None -> (
      v.nondeterministic <- List.rev v.nondeterministic;
      v.blacklisted <- List.rev v.blacklisted;
    )
    | Some (1, Pbrt.Bytes) -> (
      v.blacklisted <- (decode_path (Pbrt.Decoder.nested d)) :: v.blacklisted;
      loop ()
    )
    | Some (1, pk) -> raise (
      Protobuf.Decoder.Failure (Protobuf.Decoder.Unexpected_payload ("Message(blocking), field(1)", pk))
    )
    | Some (2, Pbrt.Bytes) -> (
      v.nondeterministic <- (decode_path (Pbrt.Decoder.nested d)) :: v.nondeterministic;
      loop ()
    )
    | Some (2, pk) -> raise (
      Protobuf.Decoder.Failure (Protobuf.Decoder.Unexpected_payload ("Message(blocking), field(2)", pk))
    )
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind; loop ()
  in
  loop ();
  let v:blocking = Obj.magic v in
  v

let rec encode_path (v:path) encoder = 
  Pbrt.Encoder.key (1, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.string v.attribute encoder;
  List.iter (fun x -> 
    Pbrt.Encoder.key (2, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.string x encoder;
  ) v.path;
  ()

let rec encode_blocking (v:blocking) encoder = 
  List.iter (fun x -> 
    Pbrt.Encoder.key (1, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.nested (encode_path x) encoder;
  ) v.blacklisted;
  List.iter (fun x -> 
    Pbrt.Encoder.key (2, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.nested (encode_path x) encoder;
  ) v.nondeterministic;
  ()

let rec pp_path fmt (v:path) = 
  let pp_i fmt () =
    Format.pp_open_vbox fmt 1;
    Pbrt.Pp.pp_record_field "attribute" Pbrt.Pp.pp_string fmt v.attribute;
    Pbrt.Pp.pp_record_field "path" (Pbrt.Pp.pp_list Pbrt.Pp.pp_string) fmt v.path;
    Format.pp_close_box fmt ()
  in
  Pbrt.Pp.pp_brk pp_i fmt ()

let rec pp_blocking fmt (v:blocking) = 
  let pp_i fmt () =
    Format.pp_open_vbox fmt 1;
    Pbrt.Pp.pp_record_field "blacklisted" (Pbrt.Pp.pp_list pp_path) fmt v.blacklisted;
    Pbrt.Pp.pp_record_field "nondeterministic" (Pbrt.Pp.pp_list pp_path) fmt v.nondeterministic;
    Format.pp_close_box fmt ()
  in
  Pbrt.Pp.pp_brk pp_i fmt ()
