module Sedlexing = Sedlexing_uutf

open Parser

let create_hashtbl num elems =
  let h = Hashtbl.create num in
  List.iter (fun (k,v) -> Hashtbl.add h k v) elems;
  h

let keyword_table =
  create_hashtbl 149 [
    "and",          AND;
    "as",           AS;
    "assert",       ASSERT;
    "begin",        BEGIN;
    "class",        CLASS;
    "constraint",   CONSTRAINT;
    "do",           DO;
    "done",         DONE;
    "downto",       DOWNTO;
    "else",         ELSE;
    "end",          END;
    "exception",    EXCEPTION;
    "external",     EXTERNAL;
    "false",        FALSE;
    "for",          FOR;
    "fun",          FUN;
    "function",     FUNCTION;
    "functor",      FUNCTOR;
    "if",           IF;
    "in",           IN;
    "include",      INCLUDE;
    "inherit",      INHERIT;
    "initializer",  INITIALIZER;
    "lazy",         LAZY;
    "let",          LET;
    "match",        MATCH;
    "method",       METHOD;
    "module",       MODULE;
    "mutable",      MUTABLE;
    "new",          NEW;
    "object",       OBJECT;
    "of",           OF;
    "open",         OPEN;
    "or",           OR;
    "private",      PRIVATE;
    "rec",          REC;
    "sig",          SIG;
    "struct",       STRUCT;
    "then",         THEN;
    "to",           TO;
    "true",         TRUE;
    "try",          TRY;
    "type",         TYPE;
    "val",          VAL;
    "virtual",      VIRTUAL;
    "when",         WHEN;
    "while",        WHILE;
    "with",         WITH;

    "mod",          INFIXOP3("mod");
    "land",         INFIXOP3("land");
    "lor",          INFIXOP3("lor");
    "lxor",         INFIXOP3("lxor");
    "lsl",          INFIXOP4("lsl");
    "lsr",          INFIXOP4("lsr");
    "asr",          INFIXOP4("asr")
  ]

type error =
| Illegal_character of int
| Invalid_UTF_8
| Char_range_exceeded

exception Error of error * Location.t

exception Unterminated

let escape_unicode uchar =
  match Uucp.Gc.general_category uchar with
  | (`Cc | `Cf | `Cn | `Co | `Cs) ->
    Printf.sprintf "U+%04d" uchar
  | _ ->
    Printf.sprintf "U+%04d %s" uchar (Sedlexing.encode [uchar])

let report_error ppf =
  function
  | Illegal_character u ->
    Format.fprintf ppf "Illegal character (%s)" (escape_unicode u)
  | Invalid_UTF_8 ->
    Format.fprintf ppf "Invalid UTF-8"
  | Char_range_exceeded ->
    Format.fprintf ppf "Unicode character exceeds range of type char"

let () =
  Location.register_error_of_exn
    (function
    | Error (err, loc) ->
      Some (Location.error_of_printer loc report_error err)
    | _ -> None)

type state = {
          lexbuf        : Sedlexing.lexbuf;
          buffer        : Buffer.t;
  mutable in_string     : bool;
  mutable comment_start : Location.t list;
}

let create lexbuf =
  { lexbuf;
    buffer = Buffer.create 16;
    in_string = false;
    comment_start = []; }

let in_comment { comment_start } = comment_start = []

(* This is necessary, as our approximation for lowercase includes
   extra characters not present in ID_Start, and we conform to ID_Start.
   See https://github.com/alainfrisch/sedlex/issues/20 *)
let check_id_start offset lexbuf =
  let id_char = List.nth (Sedlexing.lexeme lexbuf) offset in
  if not (Uucp.Id.is_id_start id_char) then
    raise (Error (Illegal_character id_char, Sedlexing.location lexbuf))

let get_label_name lexbuf =
  let name = Sedlexing.utf8_sub_lexeme ~normalize:`NFC (1, -2) lexbuf in
  if Hashtbl.mem keyword_table name then
    raise (Lexer.Error (Lexer.Keyword_as_label name, Sedlexing.location lexbuf));
  name

let char_for_uchar offset lexbuf =
  match Sedlexing.lexeme_char offset lexbuf with
  | a when a < 0xff -> Char.chr a
  | u ->
    raise (Error (Char_range_exceeded, Sedlexing.location lexbuf))

let char_for_backslash offset lexbuf =
  match Sedlexing.lexeme_char offset lexbuf with
  | 0x006e (* 'n' *) -> '\010'
  | 0x0072 (* 'r' *) -> '\013'
  | 0x0062 (* 'b' *) -> '\008'
  | 0x0074 (* 't' *) -> '\009'
  | _ -> char_for_uchar offset lexbuf

let char_for_decimal_code offset lexbuf =
  let c = 100 * ((Sedlexing.lexeme_char offset lexbuf) - 48) +
           10 * ((Sedlexing.lexeme_char (offset+1) lexbuf) - 48) +
                ((Sedlexing.lexeme_char (offset+2) lexbuf) - 48) in
  if c > 0xff then
    raise (Lexer.Error (Lexer.Illegal_escape (Sedlexing.utf8_lexeme lexbuf),
                        Sedlexing.location lexbuf))
  else
    Char.chr c

let char_for_hexadecimal_code offset lexbuf =
  let d1 = Sedlexing.lexeme_char offset lexbuf in
  let val1 = if d1 >= 97 then d1 - 87
             else if d1 >= 65 then d1 - 55
             else d1 - 48
  in
  let d2 = Sedlexing.lexeme_char (offset+1) lexbuf in
  let val2 = if d2 >= 97 then d2 - 87
             else if d2 >= 65 then d2 - 55
             else d2 - 48
  in
  Char.chr (val1 * 16 + val2)

let convert_int_literal s =
  - int_of_string ("-" ^ s)

let convert_int32_literal s =
  Int32.neg (Int32.of_string ("-" ^ String.sub s 0 (String.length s - 1)))

let convert_int64_literal s =
  Int64.neg (Int64.of_string ("-" ^ String.sub s 0 (String.length s - 1)))

let convert_nativeint_literal s =
  Nativeint.neg (Nativeint.of_string ("-" ^ String.sub s 0 (String.length s - 1)))

let with_string ({ lexbuf } as state) fn =
  Buffer.clear state.buffer;
  let string_start_loc = Sedlexing.location lexbuf in
  try
    state.in_string <- true;
    Sedlexing.expand_token lexbuf (fun () -> fn state);
    state.in_string <- false;
    Buffer.contents state.buffer
  with Unterminated ->
    state.in_string <- false;
    raise (Lexer.Error (Lexer.Unterminated_string, string_start_loc))

let blank = [%sedlex.regexp? Chars " \t\012" | 0x3000 (* IDEOGRAPHIC SPACE *)]
let uppercase = [%sedlex.regexp? lu ]
let lowercase = [%sedlex.regexp? ll | lm | lo | lt | '_' ]
let identchar = [%sedlex.regexp? id_continue | "'" ]
let symbolchar =
  [%sedlex.regexp? Chars "!$%&*+-./:<=>?@^|~" ]
let decimal_literal =
  [%sedlex.regexp? '0'..'9', Star ('0'..'9' | '_') ]
let hex_literal =
  [%sedlex.regexp? '0', Chars "xX", ('0'..'9' | 'A'..'F' | 'a'..'f'),
                                    Star ('0'..'9' | 'A'..'F' | 'a'..'f' | '_') ]
let oct_literal =
  [%sedlex.regexp? '0', Chars "oO", '0'..'7', Star ('0'..'7' | '_') ]
let bin_literal =
  [%sedlex.regexp? '0', Chars "bB", '0'..'1', Star ('0'..'1' | '_') ]
let int_literal =
  [%sedlex.regexp? decimal_literal | hex_literal | oct_literal | bin_literal ]
let float_literal =
  [%sedlex.regexp? '0'..'9', Star ('0'..'9' | '_'),
                   Opt ('.', Star ('0'..'9' | '_')),
                   Opt (Chars "eE", Opt (Chars "+-"), '0'..'9',
                        Star ('0'..'9' | '_')) ]

let rec token ({ lexbuf } as state) =
  match%sedlex lexbuf with
  | "\\\n" ->
    raise (Error (Illegal_character (List.hd (Sedlexing.lexeme lexbuf)),
                  Sedlexing.location lexbuf))
  | '\n' | '\t' | ' ' -> token state
  | '_' -> UNDERSCORE
  | '~' -> TILDE
  | uppercase, Star identchar ->
    UIDENT (Sedlexing.utf8_lexeme ~normalize:`NFC lexbuf)
  | '@', (uppercase | lowercase), Star identchar ->
    let first = Sedlexing.lexeme_char 1 lexbuf
    and rest  = Sedlexing.sub_lexeme (2, -1) lexbuf in
    let first =
      match Uucp.Case.Map.to_upper first with
      | `Self -> first
      | `Uchars [] -> assert false
      | `Uchars (mapped :: _) -> mapped
    in
    UIDENT (Sedlexing.encode ~normalize:`NFC (first :: rest))
  | '~', lowercase, Star identchar, ':' ->
    check_id_start 1 lexbuf;
    LABEL (get_label_name lexbuf)
  | '?' -> QUESTION
  | '?', lowercase, Star identchar, ':' ->
    check_id_start 1 lexbuf;
    OPTLABEL (get_label_name lexbuf)
  | lowercase, Star identchar ->
    check_id_start 0 lexbuf;
    let str = Sedlexing.utf8_lexeme ~normalize:`NFC lexbuf in
    (try Hashtbl.find keyword_table str
     with Not_found -> LIDENT str)
  | int_literal ->
    begin try
      INT (convert_int_literal (Sedlexing.utf8_lexeme lexbuf))
    with Failure _ ->
      raise (Lexer.Error (Lexer.Literal_overflow "int", Sedlexing.location lexbuf))
    end
  | float_literal ->
    FLOAT (Sedlexing.encode (List.filter ((<>) (Char.code '_')) (Sedlexing.lexeme lexbuf)))
  | int_literal, "l" ->
    begin try
      INT32 (convert_int32_literal (Sedlexing.utf8_lexeme lexbuf))
    with Failure _ ->
      raise (Lexer.Error (Lexer.Literal_overflow "int32", Sedlexing.location lexbuf))
    end
  | int_literal, "L" ->
    begin try
      INT64 (convert_int64_literal (Sedlexing.utf8_lexeme lexbuf))
    with Failure _ ->
      raise (Lexer.Error (Lexer.Literal_overflow "int64", Sedlexing.location lexbuf))
    end
  | int_literal, "n" ->
    begin try
      NATIVEINT (convert_nativeint_literal (Sedlexing.utf8_lexeme lexbuf))
    with Failure _ ->
      raise (Lexer.Error (Lexer.Literal_overflow "nativeint", Sedlexing.location lexbuf))
    end
  | '"' ->
    STRING (with_string state string, None)
  | '{', Star lowercase, '|' ->
    let delim = Sedlexing.utf8_sub_lexeme ~normalize:`NFC (1, -2) lexbuf in
    STRING (with_string state (delimited_string delim), Some delim)
  | "'\n'" -> CHAR '\n'
  | "'", Compl ('\\' | '\''), "'" ->
    CHAR (char_for_uchar 1 lexbuf)
  | "'\\", Chars "\\'\"ntbr ", "'" ->
    CHAR (char_for_backslash 2 lexbuf)
  | "'\\", '0'..'9', '0'..'9', '0'..'9', "'" ->
    CHAR (char_for_decimal_code 2 lexbuf)
  | "'\\x", ('0'..'9' | 'a'..'f' | 'A'..'F'),
            ('0'..'9' | 'a'..'f' | 'A'..'F'), "'" ->
    CHAR (char_for_hexadecimal_code 3 lexbuf )
  | '\\', any ->
    raise (Lexer.Error (Lexer.Illegal_escape (Sedlexing.utf8_sub_lexeme (1, 0) lexbuf),
                        Sedlexing.location lexbuf))
  | "(*" ->
    state.comment_start <- [Sedlexing.location lexbuf];
    comment state
  | "(*)" ->
    let loc = Sedlexing.location lexbuf in
    if !Lexer.print_warnings then
      Location.prerr_warning loc Warnings.Comment_start;
    state.comment_start <- [loc];
    comment state
  | "*)" ->
    let loc = Sedlexing.location lexbuf in
    Location.prerr_warning loc Warnings.Comment_not_end;
    Sedlexing.unshift lexbuf;
    STAR
  | "#", Star (Chars " \t"), Plus ('0'..'9'),
         Star (Chars " \t"), '"', Star (Compl ('"' | '\n')), '"',
         Star (Compl '\n'), '\n' ->
    let lexeme = Sedlexing.utf8_lexeme lexbuf in
    Scanf.sscanf lexeme "# %d \"%s@\"\"" (fun line file ->
      Sedlexing.set_position lexbuf file line);
    token state
  | "#"  -> SHARP
  | "&"  -> AMPERSAND
  | "&&" -> AMPERAMPER
  | "`"  -> BACKQUOTE
  | "'"  -> QUOTE
  | "("  -> LPAREN
  | ")"  -> RPAREN
  | "*"  -> STAR
  | ","  -> COMMA
  | "->" -> MINUSGREATER
  | "."  -> DOT
  | ".." -> DOTDOT
  | ":"  -> COLON
  | "::" -> COLONCOLON
  | ":=" -> COLONEQUAL
  | ":>" -> COLONGREATER
  | ";"  -> SEMI
  | ";;" -> SEMISEMI
  | "<"  -> LESS
  | "<-" -> LESSMINUS
  | "="  -> EQUAL
  | "["  -> LBRACKET
  | "[|" -> LBRACKETBAR
  | "[<" -> LBRACKETLESS
  | "[>" -> LBRACKETGREATER
  | "]"  -> RBRACKET
  | "{"  -> LBRACE
  | "{<" -> LBRACELESS
  | "|"  -> BAR
  | "||" -> BARBAR
  | "|]" -> BARRBRACKET
  | ">"  -> GREATER
  | ">]" -> GREATERRBRACKET
  | "}"  -> RBRACE
  | ">}" -> GREATERRBRACE
  | "[@" -> LBRACKETAT
  | "[%" -> LBRACKETPERCENT
  | "[%%" -> LBRACKETPERCENTPERCENT
  | "[@@" -> LBRACKETATAT
  | "[@@@" -> LBRACKETATATAT
  | "!"  -> BANG
  | "!=" -> INFIXOP0 "!="
  | "+"  -> PLUS
  | "+." -> PLUSDOT
  | "+=" -> PLUSEQ
  | "-"  -> MINUS
  | "-." -> MINUSDOT

  | "!", Plus symbolchar ->
    PREFIXOP (Sedlexing.utf8_lexeme lexbuf)
  | Chars "~?", Plus symbolchar ->
    PREFIXOP (Sedlexing.utf8_lexeme lexbuf)
  | Chars "=<>|&$", Star symbolchar ->
    INFIXOP0 (Sedlexing.utf8_lexeme lexbuf)
  | Chars "@^", Star symbolchar ->
    INFIXOP1 (Sedlexing.utf8_lexeme lexbuf)
  | Chars "+-", Star symbolchar ->
    INFIXOP2 (Sedlexing.utf8_lexeme lexbuf)
  | "**", Star symbolchar ->
    INFIXOP4 (Sedlexing.utf8_lexeme lexbuf)
  | '%' -> PERCENT
  | Chars "*/%", Star symbolchar ->
    INFIXOP3 (Sedlexing.utf8_lexeme lexbuf)
  | eof -> EOF
  | 0xfffd ->
    raise (Error (Invalid_UTF_8, Sedlexing.location lexbuf))
  | any ->
    raise (Error (Illegal_character (List.hd (Sedlexing.lexeme lexbuf)),
                  Sedlexing.location lexbuf))
  | _ -> assert false (* https://github.com/alainfrisch/sedlex/issues/16 *)

(* Note that COMMENT token is never emitted, as it is not actually used
   by the parser (or any tools for that matter). *)
and comment ({ lexbuf } as state) =
  let unterminated_string str_start =
    match state.comment_start with
    | [] -> assert false
    | loc :: _ ->
      let start = List.hd (List.rev state.comment_start) in
      state.comment_start <- [];
      raise (Lexer.Error (Lexer.Unterminated_string_in_comment (start, str_start),
                          loc))
  in
  match%sedlex lexbuf with
  | "(*" ->
    state.comment_start <- (Sedlexing.location lexbuf) :: state.comment_start;
    comment state
  | "*)" ->
    begin match state.comment_start with
    | [] -> assert false
    | [_] -> state.comment_start <- []; token state
    | _ :: l -> state.comment_start <- l; comment state
    end
  | "\"" ->
    let str_start = Sedlexing.location lexbuf in
    (try string state with Unterminated -> unterminated_string str_start);
    comment state
  | '{', Star lowercase, '|' ->
    let delim = Sedlexing.utf8_sub_lexeme ~normalize:`NFC (1, -2) lexbuf
    and str_start = Sedlexing.location lexbuf in
    (try delimited_string delim state with Unterminated -> unterminated_string str_start);
    comment state
  | eof ->
    begin match state.comment_start with
    | [] -> assert false
    | loc :: _ ->
      let start = List.hd (List.rev state.comment_start) in
      state.comment_start <- [];
      raise (Lexer.Error (Lexer.Unterminated_comment start, loc))
    end
  | any -> comment state
  | _ -> assert false

and string ({ lexbuf; buffer } as state) =
  match%sedlex lexbuf with
  | '"' -> ()
  | "\\\n", Star zs -> string state
  | "'", Compl ('\\' | '\''), "'" ->
    Buffer.add_char buffer (char_for_uchar 1 lexbuf)
  | "'\\", Chars "\\'\"ntbr ", "'" ->
    Buffer.add_char buffer (char_for_backslash 2 lexbuf)
  | "'\\", '0'..'9', '0'..'9', '0'..'9', "'" ->
    begin try
      Buffer.add_char buffer (char_for_decimal_code 2 lexbuf)
    with Lexer.Error (Lexer.Illegal_escape _, _) ->
      ()
    end
  | "'\\x", ('0'..'9' | 'a'..'f' | 'A'..'F'),
            ('0'..'9' | 'a'..'f' | 'A'..'F'), "'" ->
    Buffer.add_char buffer (char_for_hexadecimal_code 3 lexbuf )
  | '\\', any ->
    if in_comment state then
      string state
    else
      raise (Lexer.Error (Lexer.Illegal_escape (Sedlexing.utf8_lexeme lexbuf),
                          Sedlexing.location lexbuf))
  | '\n' ->
    if not (in_comment state) then
      Location.prerr_warning (Sedlexing.location lexbuf) Warnings.Eol_in_string;
    Buffer.add_char buffer '\n';
    string state
  | eof ->
    raise Unterminated
  | any ->
    Buffer.add_string buffer (Sedlexing.utf8_lexeme lexbuf);
    string state
  | _ -> assert false

and delimited_string delim ({ lexbuf; buffer } as state) =
  match%sedlex lexbuf with
  | '\n' ->
    Buffer.add_char buffer '\n';
    delimited_string delim state
  | eof ->
    raise Unterminated
  | "|", Star lowercase, "}" ->
    let edelim = Sedlexing.utf8_sub_lexeme ~normalize:`NFC (1, -2) lexbuf in
    if delim = edelim then ()
    else begin
      Buffer.add_string buffer (Sedlexing.utf8_lexeme lexbuf);
      delimited_string delim state
    end
  | any ->
    Buffer.add_string buffer (Sedlexing.utf8_lexeme lexbuf);
    delimited_string delim state
  | _ -> assert false

and skip_sharp_bang { lexbuf } =
  match%sedlex lexbuf with
  | "#!", Star (Compl '\n'), '\n', Star (Compl '\n'), "\n!#\n" -> ()
  | "#!", Star (Compl '\n'), '\n' -> ()
  | _ -> ()

(* type position' = [%import: Lexing.position] [@@deriving show]
type location' = [%import: Location.t [@with Lexing.position := position']] [@@deriving show]
type token' = [%import: Parser.token [@with Location.t := location']] [@@deriving show]
 *)

let wrap fn =
  fun state oldlexbuf ->
    let token = fn state in
    Sedlexing_uutf.fill_lexbuf state.lexbuf oldlexbuf;
    (* prerr_endline (show_token' token); *)
    token

let token' = wrap token

