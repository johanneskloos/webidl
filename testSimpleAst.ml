type mode = Step1 | Step2 | Step3
let pp_definitions =
  let open Fmt in
    vbox (suffix cut (list ~sep:cut SimpleAst.pp_definition))
let pp_scoped_name_map sep pp_value =
  let open Fmt in
    iter_bindings AstSimplify.ScopedNameMap.iter
      (pair ~sep SimpleAst.pp_scoped_name pp_value)
let pp_typedef =
  pp_scoped_name_map (Fmt.const Fmt.string "=")
    (SimpleAst.pp_with_attributes SimpleAst.pp_type_)
let pp_structure_resolution pp { AstSimplify.sr_definitions; sr_typedefs } =
  let open Fmt in
  pf pp "@[<v>Typedefs:@ %a@ @ %a@]@."
    pp_typedef sr_typedefs
    pp_definitions sr_definitions
let pp_module_resolution pp
      { AstSimplify.mr_definitions; mr_typedefs;
        mr_partial_dictionary; mr_partial_interface } =
  let open Fmt in
    pf pp "@[<v>Typedefs:@ %a@ @ Partial dictionaries:@ %a@ @ Partial interfaces:@ %a@ @ %a@]@."
      pp_typedef mr_typedefs
      (pp_scoped_name_map cut (vbox (list SimpleAst.pp_dictionary))) mr_partial_dictionary
      (pp_scoped_name_map cut (vbox (list SimpleAst.pp_interface))) mr_partial_interface
      pp_definitions mr_definitions

let () =
  let open Arg in
  let files = ref []
  and mode = ref Step3 in
    Arg.parse [
      ("-1", Unit (fun () -> mode := Step1), "Only perform step 1");
      ("-2", Unit (fun () -> mode := Step2), "Only perform steps 1 and 2")
    ] (fun arg -> files := arg :: !files)
      "testSimpleAst filenames";
    let asts =
      List.rev_map (fun filename -> Parse.read_ast_from_file filename) !files
    in let ast = List.concat asts
    in match !mode with
      | Step3 -> 
          pp_definitions Format.std_formatter (AstSimplify.cleanup ast)
      | Step2 ->
          let ctx = ContextError.ctx_top () in
          let res = AstSimplify.step1 ctx (List.concat asts) |> AstSimplify.step2 ctx
          in ContextError.flush_errors_and_handle_failure ctx;
             pp_structure_resolution Format.std_formatter res
      | Step1 ->
          let ctx = ContextError.ctx_top () in
          let res = AstSimplify.step1 ctx (List.concat asts)
          in ContextError.flush_errors_and_handle_failure ctx;
             pp_module_resolution Format.std_formatter res

