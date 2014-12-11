let wrap_parser ?load_path fn lexbuf =
  let state = M17n_lexer.create ?load_path lexbuf in
  let oldlexbuf = Lexing.from_string "" in
  let ast = fn (M17n_lexer.token' state) oldlexbuf in
  Parsing.clear_parser ();
  ast

let () =
  let filename, load_path =
    let filename = ref "" in
    let load_path = ref [] in
    Arg.parse [
        "-I", Arg.String (fun x -> load_path := x :: !load_path), "<path> add <path> to load path";
        "-ignore", Arg.Unit ignore, "ignored";
      ]
      (fun arg -> filename := arg)
      "OCaml Multilingualization preprocessor";
    !filename, load_path
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
      let ast = wrap_parser ~load_path Parser.interface lexbuf in
      output_string stdout Config.ast_intf_magic_number;
      output_value  stdout filename;
      output_value  stdout ast
    else if Filename.check_suffix filename ".ml" then
      let ast = wrap_parser ~load_path Parser.implementation lexbuf in
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
