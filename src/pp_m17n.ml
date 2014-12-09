let wrap_parser fn lexbuf =
  let state = Ulexer.create lexbuf in
  let oldlexbuf = Lexing.from_string "" in
  let ast = fn (Ulexer.token' state) oldlexbuf in
  Parsing.clear_parser ();
  ast

let () =
  let filename = Sys.argv.(1) in
  let chan = open_in filename in
  let input =
    let buf = Bytes.create 4096 in
    fun () ->
      match input chan buf 0 (Bytes.length buf) with
      | 0 -> None
      | n -> Some (Bytes.sub_string buf 0 n)
  in
  let lexbuf = Sedlexing_uutf.create ~filename input in
  try
    let ast = wrap_parser Parser.implementation lexbuf in
    ignore ast
  with exn ->
    Location.report_exception Format.err_formatter exn;
    exit 1
