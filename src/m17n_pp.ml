let wrap_parser ?include_paths fn lexbuf =
  let state = M17n_lexer.create ?include_paths lexbuf in
  let oldlexbuf = Lexing.from_string "" in
  let ast = fn (M17n_lexer.token' state) oldlexbuf in
  Parsing.clear_parser ();
  ast

let dynlink include_paths file =
  let file = Dynlink.adapt_filename file in
  try
    try
      (* First, try to load it directly. *)
      Dynlink.loadfile file
    with Dynlink.Error (Dynlink.File_not_found _) ->
      (* Now, look it up in the load path. We only really need the path to the m17n package,
         but there's no especially good way to get it, and the preprocessor is guaranteed
         to have the path to the m17n package passed through a -I option, and there's
         no real possibility of name conflicts, so whatever. *)
      !include_paths |> List.iter (fun include_path ->
        try
          Dynlink.loadfile (Filename.concat include_path file);
          raise Exit
        with Dynlink.Error (Dynlink.File_not_found _) ->
          ())
  with
  | Dynlink.Error error ->
      invalid_arg (Dynlink.error_message error)
  | Exit -> ()

let () =
  let filename, include_paths =
    let filename = ref "" in
    let include_paths = ref [] in
    Arg.parse [
        "-I", Arg.String (fun x -> include_paths := x :: !include_paths),
          "<path> add <path> to compiler load paths";
        "-load", Arg.String (dynlink include_paths),
          "<file> dynamically load the file <file>";
        "-ignore", Arg.Unit ignore, "ignored";
      ]
      (fun arg -> filename := arg)
      "OCaml Multilingualization preprocessor";
    !filename, include_paths
  in
  let chan = open_in filename in
  let input =
    let buf = Bytes.create 4096 in
    fun () ->
      match input chan buf 0 (Bytes.length buf) with
      | 0 -> None
      | n -> Some (buf, 0, n)
  in
  let lexbuf = M17n_sedlexing.create ~filename input in
  try
    if Filename.check_suffix filename ".mli" then
      let ast = wrap_parser ~include_paths Parser.interface lexbuf in
      output_string stdout Config.ast_intf_magic_number;
      output_value  stdout filename;
      output_value  stdout ast
    else if Filename.check_suffix filename ".ml" then
      let ast = wrap_parser ~include_paths Parser.implementation lexbuf in
      output_string stdout Config.ast_impl_magic_number;
      output_value  stdout filename;
      output_value  stdout ast
    else
      (prerr_string ("Don't know what to do with " ^ filename ^ ".");
       exit 1)
  with
  | Parsing.Parse_error | Syntaxerr.Escape_error ->
    let exn = Syntaxerr.Error (Syntaxerr.Other (M17n_sedlexing.location lexbuf)) in
    Location.report_exception Format.err_formatter exn;
  | exn ->
    Location.report_exception Format.err_formatter exn;
    exit 1
