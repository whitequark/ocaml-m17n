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

let wrap_parser fn oldlexbuf =
  let open Lexing in
  let rec gen () =
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
  in
  let kind = if !Toploop.input_name = "//toplevel//" then `Toplevel else `Batch in
  let lexbuf = M17n_sedlexing.create ~kind ~filename:!Toploop.input_name gen in
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

let () =
  Toploop.parse_toplevel_phrase := wrap_parser Parser.toplevel_phrase;
  Toploop.parse_use_file := wrap_parser Parser.use_file;
  prerr_endline "OCaml Multilingualization enabled."
