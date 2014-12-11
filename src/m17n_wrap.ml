let gen_of_lexbuf oldlexbuf () =
  let open Lexing in
  if oldlexbuf.lex_eof_reached &&
     oldlexbuf.lex_curr_pos = oldlexbuf.lex_buffer_len then
    None
  else begin
    if oldlexbuf.lex_curr_pos = oldlexbuf.lex_buffer_len then
      oldlexbuf.refill_buff oldlexbuf;
    let curr_pos = oldlexbuf.lex_curr_pos in
    oldlexbuf.lex_curr_pos <- oldlexbuf.lex_buffer_len;
    Some (oldlexbuf.lex_buffer, curr_pos,
          oldlexbuf.lex_buffer_len - curr_pos)
  end

let rec skip_phrase state =
  try
    match M17n_lexer.token state with
    | Parser.SEMISEMI | Parser.EOF -> ()
    | _ -> skip_phrase state
  with
  | Lexer.Error (Lexer.Unterminated_comment _, _)
  | Lexer.Error (Lexer.Unterminated_string, _)
  | Lexer.Error (Lexer.Unterminated_string_in_comment _, _)
  | Lexer.Error (Lexer.Illegal_character _, _) ->
    skip_phrase state

let maybe_skip_phrase state =
  if Parsing.is_current_lookahead Parser.SEMISEMI
  || Parsing.is_current_lookahead Parser.EOF
  then ()
  else skip_phrase state

let internationalize fn oldlexbuf =
  let kind = if !Toploop.input_name = "//toplevel//" then `Toplevel else `Batch in
  let lexbuf = M17n_sedlexing.create ~kind ~filename:!Toploop.input_name
                                     (gen_of_lexbuf oldlexbuf) in
  let state = M17n_lexer.create lexbuf in
  try
    (* toplevel's Location is inaccessible (expunged); sync data with ours *)
    let ast = fn (M17n_lexer.token' state) oldlexbuf in
    Parsing.clear_parser ();
    ast
  with
  | Lexer.Error(Lexer.Illegal_character _, _) as err
      when !Toploop.input_name = "//toplevel//" ->
    skip_phrase state;
    raise err
  | Syntaxerr.Error _ as err
      when !Toploop.input_name = "//toplevel//" ->
    maybe_skip_phrase state;
    raise err
  | Parsing.Parse_error | Syntaxerr.Escape_error ->
    let loc = M17n_sedlexing.location lexbuf in
    if !Toploop.input_name = "//toplevel//" then
      maybe_skip_phrase state;
    raise (Syntaxerr.Error (Syntaxerr.Other loc))

let utf8_parenthesized_ident name =
  (List.mem name ["or"; "mod"; "land"; "lor"; "lxor"; "lsl"; "lsr"; "asr"]) ||
  (match name.[0] with
   | 'a'..'z' | 'A'..'Z' | '_' | '\128'..'\255' -> false
   | _ -> true)

let utf8_print_string fmt s =
  try
    try
      let uutf = Uutf.decoder ~encoding:`UTF_8 (`String s) in
      let buf  = Buffer.create (String.length s) in
      let rec loop () =
        match Uutf.decode uutf with
        | `Malformed _ -> raise Exit
        | `End -> ()
        | `Uchar u ->
          begin match Uucp.Gc.general_category u with
          | (`Cc | `Cf | `Cn | `Co | `Cs) -> raise Exit
          | _ -> Uutf.Buffer.add_utf_8 buf u
          end;
          loop ()
        | `Await -> assert false
      in
      Buffer.add_char buf '"';
      loop ();
      Buffer.add_char buf '"';
      Format.pp_print_string fmt (Buffer.contents buf)
    with Exit ->
      Format.fprintf fmt "%S" s
  with Invalid_argument "String.create" ->
    Format.fprintf fmt "<huge string>"

let utf8_value_ident ppf name =
  if utf8_parenthesized_ident name then
    Format.fprintf ppf "( %s )" name
  else
    Format.pp_print_string ppf name

let utf8_print_out_sig_item next ppf =
  function
  | Outcometree.Osig_value (name, ty, prims) ->
    let kwd = if prims = [] then "val" else "external" in
    let pr_prims ppf =
      function
        [] -> ()
      | s :: sl ->
          Format.fprintf ppf "@ = \"%s\"" s;
          List.iter (fun s -> Format.fprintf ppf "@ \"%s\"" s) sl
    in
    Format.fprintf ppf "@[<2>%s %a :@ %a%a@]" kwd utf8_value_ident name !Toploop.print_out_type
      ty pr_prims prims
  | x -> next ppf x
