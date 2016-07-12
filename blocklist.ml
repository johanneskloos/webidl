module B = CalculateBlocking.Blacklist
module N = CalculateBlocking.Nondeterministic
open TypeGraph

let to_path (path, attribute) =
  { Blocking_pb.attribute; Blocking_pb.path }

let () =
  let open Arg in Format.set_margin max_int;
  let files = ref [] and outfile = ref None in
    Arg.parse [("-o", String (fun s -> outfile := Some s), "Output file")]
      (fun arg -> files := arg :: !files)
      "testGraph filenames";
    let defs = Webidl.flatten_from_files !files
    in let graph = TypeGraph.build_reachability_graph defs
    in let paths_blacklist = B.calculate_blocking_paths graph
    and paths_nondeterministic = N.calculate_blocking_paths graph
    and encoder = Protobuf.Encoder.create()
    in let blocking = {
      Blocking_pb.blacklisted = BatList.map to_path paths_blacklist;
      Blocking_pb.nondeterministic = BatList.map to_path paths_nondeterministic
    }
    in Blocking_pb.encode_blocking blocking encoder;
       let bytes = Protobuf.Encoder.to_bytes encoder in
       match !outfile with
         | Some fn ->
             let chan = open_out fn in begin
               try output_bytes chan bytes; close_out chan
               with e -> close_out chan; raise e
             end
         | None -> output_bytes stdout bytes

