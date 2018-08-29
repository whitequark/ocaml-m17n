module Sedlexing = M17n_sedlexing

open Parser

let create_hashtbl num elems =
  let h = Hashtbl.create num in
  List.iter (fun (k,v) -> Hashtbl.add h k v) elems;
  h

let keywords =
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
| Illegal_character of Uchar.t
| Invalid_UTF_8
| Char_range_exceeded

exception Error of error * Location.t

exception Unterminated

let escape_unicode uchar =
  match Uucp.Gc.general_category uchar with
  | (`Cc | `Cf | `Cn | `Co | `Cs) ->
    Format.asprintf "%a" Uchar.dump uchar
  | _ ->
    Format.asprintf "%a %s" Uchar.dump uchar (Sedlexing.encode [uchar])

let report_error fmt =
  function
  | Illegal_character u ->
    Format.fprintf fmt "Illegal character (%s)" (escape_unicode u)
  | Invalid_UTF_8 ->
    Format.fprintf fmt "Invalid UTF-8"
  | Char_range_exceeded ->
    Format.fprintf fmt "Unicode character exceeds range of type char"

let () =
  Location.register_error_of_exn
    (function
    | Error (err, loc) ->
      Some (Location.error_of_printer loc report_error err)
    | _ -> None)

let report_warnings fmt warnings =
  warnings |> List.iter (fun (loc, warning) ->
    Location.print_warning loc fmt warning)

let toNFKC_casefold uchars =
  let uunf = Uunf.create `NFC in
  let rec add uchar acc =
    match Uunf.add uunf uchar with
    | `Uchar uchar' -> add `Await (uchar' :: acc)
    | `Await | `End -> acc
  in
  uchars |> List.fold_left (fun acc uchar ->
    match Uucp.Case.Nfkc_fold.fold uchar with
    | `Self -> add (`Uchar uchar) acc
    | `Uchars uchars' ->
      List.fold_left (fun acc' uchar' -> add (`Uchar uchar') acc') acc uchars') [] |>
  add `End |>
  List.rev

let to_skeleton uchars =
  let rec bsearch a b needle =
    if a = b then None
    else
      let m = a + (b - a) / 2 in
      let (uchar, mapping) = M17n_confusable_gen.ma.(m) in
      if uchar = needle then Some mapping
      else if uchar > needle then bsearch a m needle
      else bsearch (m + 1) b needle
  in
  let find uchar =
    bsearch 0 (Array.length M17n_confusable_gen.ma) uchar
  in
  uchars |>
  List.fold_left (fun acc uchar ->
    match find (Uchar.to_int uchar) with
    | None -> uchar :: acc
    | Some mapping ->
      Array.fold_left (fun acc uchar -> Uchar.of_int uchar :: acc) acc mapping) [] |>
  List.rev

type state = {
          lexbuf        : Sedlexing.lexbuf;
          buffer        : Buffer.t;
  mutable in_string     : bool;
  mutable comment_start : Location.t list;

          ident_locs    : (Uchar.t list, string * Location.t) Hashtbl.t;
                          (* after TR39 skeleton/original/location *)
          cmis_in_scope : (Uchar.t list, string) Hashtbl.t;
                          (* after/before toNFKC_Casefold *)
  mutable uident_state  : [ `Nondot | `Dot | `Uident ];
}

let find_cmis include_paths =
  let hashtbl = Hashtbl.create 16 in
  include_paths |>
  List.fold_left (fun acc dirname ->
    try
      let dir = Unix.opendir dirname in
      let rec loop rest =
        try
          let filename = Unix.readdir dir in
          if Filename.check_suffix filename ".cmi" then
            loop (Filename.chop_extension filename :: rest)
          else
            loop rest
        with End_of_file ->
          rest
      in
      loop acc
    with Unix.Unix_error _ ->
      acc) [] |>
  List.iter (fun basename ->
    try
      let uchars =
        Uutf.String.fold_utf_8 (fun acc _ ->
          function
          | `Malformed _ -> raise Exit
          | `Uchar u -> u :: acc) [] basename
      in
      Hashtbl.add hashtbl (toNFKC_casefold (List.rev uchars)) basename
    with Exit ->
      ());
  hashtbl

let create ?(include_paths=ref []) lexbuf =
  { lexbuf;
    buffer = Buffer.create 16;
    in_string = false;
    comment_start = [];
    ident_locs = Hashtbl.create 16;
    uident_state = `Nondot;
    cmis_in_scope = find_cmis !include_paths; }

let in_comment { comment_start } = comment_start = []

(* This is necessary, as our approximation for lowercase includes
   extra characters not present in ID_Start, and we conform to ID_Start.
   See https://github.com/alainfrisch/sedlex/issues/20 *)
let check_id_start offset lexbuf =
  let id_char = List.nth (Sedlexing.lexeme lexbuf) offset in
  match id_char with
  | _ when Uchar.to_int id_char = 0x005F (* LOW LINE *) -> ()
  | _ when (Uucp.Id.is_id_start id_char) -> ()
  | _ ->
    raise (Error (Illegal_character id_char, Sedlexing.location lexbuf))

let check_mixed_script exts =
  let exts = List.sort_uniq compare exts in
  (* Are all characters from a single script? *)
  match exts with
  | [] -> `Single
  | ext :: rest ->
    let soss = List.fold_left (fun soss exts ->
                  List.filter (fun s -> List.exists ((=) s) exts) soss) ext rest in
    if soss <> [] then
      `Single
    else
      let covers scripts =
        List.for_all (fun ext ->
          List.filter (fun s -> List.exists ((=) s) scripts) ext <> []) exts
      in
      (* Is it covered by:
          * Han + Hiragana + Katakana;
          * Han + Bopomofo; or
          * Han + Hangul? *)
      if covers [`Hani;`Hira;`Kana] || covers [`Hani;`Bopo] || covers [`Hani;`Hang] then
        `Single
      else
        `Mixed (List.sort_uniq compare (List.concat exts))

let check_mixed_script_chunks uchars =
  uchars |>
  (* Map characters to script extensions, ignoring Common and Inherited
     scripts, i.e. compute Set Of Script Sets. However, keep _ as-is. *)
  List.fold_left (fun acc uchar ->
    match Uucp.Script.script_extensions uchar with
    | [`Zinh] | [`Zyyy] | [`Zzzz] when Uchar.to_int uchar <> 0x005F (* LOW LINE *) -> acc
    | scripts -> scripts :: acc) [] |>
  (* Divide the identifier into chunks separated by _ and verify they
     all comply with Highly Restrictive. *)
  let rec loop chunk exts =
    match exts with
    | [] -> check_mixed_script chunk
    | [`Zyyy] :: exts ->
      begin match check_mixed_script chunk with
      | `Single -> loop [] exts
      | mixed  -> mixed
      end
    | ext :: exts -> loop (ext :: chunk) exts
  in
  loop []

let check_confusable_identifier lexbuf ident_locs =
  let lexeme = Sedlexing.lexeme lexbuf in
  let utf8_lexeme = Sedlexing.encode ~normalize:`NFC lexeme in
  begin match check_mixed_script_chunks lexeme with
  | `Single -> ()
  | `Mixed scripts ->
    let msg = "Identifier " ^ utf8_lexeme ^ " contains a mixed script sequence" ^
              " (" ^ (String.concat ", " (List.map
                        (fun x -> Format.asprintf "%a" Uucp.Script.pp x) scripts)) ^ ")" in
    report_warnings Format.err_formatter [Sedlexing.location lexbuf, Warnings.Preprocessor msg]
  end;
  let skeleton = to_skeleton lexeme in
  try
    let utf8_lexeme', location' = Hashtbl.find ident_locs skeleton in
    if utf8_lexeme' <> utf8_lexeme then begin
      let msg = "Identifier " ^ utf8_lexeme ^ " looks confusingly similar to another" in
      report_warnings Format.err_formatter [Sedlexing.location lexbuf, Warnings.Preprocessor msg];
      let msg = "Identifier " ^ utf8_lexeme' ^ " defined earlier" in
      report_warnings Format.err_formatter [location', Warnings.Preprocessor msg]
    end
  with Not_found ->
    Hashtbl.add ident_locs skeleton (utf8_lexeme, Sedlexing.location lexbuf)

let get_label_name lexbuf =
  let name = Sedlexing.utf8_sub_lexeme ~normalize:`NFC (1, -2) lexbuf in
  if Hashtbl.mem keywords name then
    raise (Lexer.Error (Lexer.Keyword_as_label name, Sedlexing.location lexbuf));
  name

let char_for_uchar offset lexbuf =
  match Sedlexing.lexeme_char offset lexbuf with
  | a when Uchar.to_int a < 0xff -> Uchar.to_char a
  | u ->
    raise (Error (Char_range_exceeded, Sedlexing.location lexbuf))

let char_for_backslash offset lexbuf =
  match Uchar.to_int (Sedlexing.lexeme_char offset lexbuf) with
  | 0x006e (* 'n' *) -> '\010'
  | 0x0072 (* 'r' *) -> '\013'
  | 0x0062 (* 'b' *) -> '\008'
  | 0x0074 (* 't' *) -> '\009'
  | _ -> char_for_uchar offset lexbuf

let char_for_decimal_code offset lexbuf =
  let digit_value_at offset =
    (Uchar.to_int (Sedlexing.lexeme_char offset lexbuf)) - 48
  in
  let c = 100 * (digit_value_at offset) +
           10 * (digit_value_at (offset + 1)) +
                (digit_value_at (offset + 2)) in
  if c > 0xff then
    raise (Lexer.Error (Lexer.Illegal_escape (Sedlexing.utf8_lexeme lexbuf),
                        Sedlexing.location lexbuf))
  else
    Char.chr c

let char_for_hexadecimal_code offset lexbuf =
  let digit_value_at offset =
    let d = Uchar.to_int (Sedlexing.lexeme_char offset lexbuf) in
    if d >= 97 then d - 87
    else if d >= 65 then d - 55
    else d - 48
  in
  Char.chr ((digit_value_at offset) * 16 +
            (digit_value_at (offset + 1)))

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

let rec token ({ lexbuf; ident_locs } as state) =
  match%sedlex lexbuf with
  | "\\\n" ->
    raise (Error (Illegal_character (List.hd (Sedlexing.lexeme lexbuf)),
                  Sedlexing.location lexbuf))
  | '\n' | '\t' | ' ' -> token state
  | '_' -> UNDERSCORE
  | '~' -> TILDE
  | uppercase, Star identchar ->
    check_confusable_identifier lexbuf ident_locs;
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
    check_confusable_identifier lexbuf ident_locs;
    UIDENT (Sedlexing.encode ~normalize:`NFC (first :: rest))
  | '~', lowercase, Star identchar, ':' ->
    check_id_start 1 lexbuf;
    check_confusable_identifier lexbuf ident_locs;
    LABEL (get_label_name lexbuf)
  | '?' -> QUESTION
  | '?', lowercase, Star identchar, ':' ->
    check_id_start 1 lexbuf;
    check_confusable_identifier lexbuf ident_locs;
    OPTLABEL (get_label_name lexbuf)
  | lowercase, Star identchar ->
    check_id_start 0 lexbuf;
    let str = Sedlexing.utf8_lexeme ~normalize:`NFC lexbuf in
    begin try
      Hashtbl.find keywords str
    with Not_found ->
      check_confusable_identifier lexbuf ident_locs;
      LIDENT str
    end
  | int_literal ->
    INT (Sedlexing.utf8_lexeme lexbuf, None)
  | int_literal, ('A'..'Z' | 'a'..'z') ->
    INT (Sedlexing.utf8_sub_lexeme (0, -2) lexbuf,
         Some (Uchar.to_char (List.hd (List.rev (Sedlexing.lexeme lexbuf)))))
  | float_literal ->
    FLOAT (Sedlexing.encode (List.filter ((<>) (Uchar.of_char '_')) (Sedlexing.lexeme lexbuf)),
           None)
  | float_literal, ('A'..'Z' | 'a'..'z') ->
    FLOAT (Sedlexing.encode (List.filter ((<>) (Uchar.of_char '_')) (Sedlexing.lexeme lexbuf)),
           Some (Uchar.to_char (List.hd (List.rev (Sedlexing.lexeme lexbuf)))))
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
#if OCAML_VERSION >= (4, 04, 0)
  | "#"  -> HASH
#else
  | "#"  -> SHARP
#endif
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
  | "\\\n", Star ('\t' | '\n') -> string state
  | "\\", Chars "\\'\"ntbr ", "'" ->
    Buffer.add_char buffer (char_for_backslash 1 lexbuf); string state
  | "\\", '0'..'9', '0'..'9', '0'..'9' ->
    begin try
      Buffer.add_char buffer (char_for_decimal_code 1 lexbuf)
    with Lexer.Error (Lexer.Illegal_escape _, _) when in_comment state ->
      ()
    end;
    string state
  | "\\x", ('0'..'9' | 'a'..'f' | 'A'..'F'),
           ('0'..'9' | 'a'..'f' | 'A'..'F') ->
    Buffer.add_char buffer (char_for_hexadecimal_code 2 lexbuf);
    string state
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

(* Wrap [token] in a little state machine to find all references to
   external modules. This is similar to the algorithm used by ocamldep. *)
let token ({ lexbuf; cmis_in_scope } as state) =
  let t = token state in
  match state.uident_state, t with
  | `Nondot, UIDENT ident ->
    let uchars =
      (* Strip leading @, if any *)
      match Sedlexing.lexeme lexbuf with
      | first :: rest when Uchar.to_int first = 0x0040 -> rest
      | uchars -> uchars
    in
    let uchars = toNFKC_casefold uchars in
    begin try
      let ident' = Hashtbl.find cmis_in_scope uchars in
      if ident <> ident' then
        let msg = "Identifier " ^ ident ^ " is similar but distinct from " ^ ident' ^
                  ",\nwhich corresponds to a module in load path" in
        report_warnings Format.err_formatter [Sedlexing.location lexbuf, Warnings.Preprocessor msg]
    with Not_found ->
      ()
    end;
    state.uident_state <- `Uident; t
  | `Nondot, _ -> t
  | `Uident, DOT ->
    state.uident_state <- `Dot; t
  | `Dot, UIDENT _ ->
    state.uident_state <- `Uident; t
  | (`Uident | `Dot), _ ->
    state.uident_state <- `Nondot; t

(* type position' = [%import: Lexing.position] [@@deriving show]
type location' = [%import: Location.t [@with Lexing.position := position']] [@@deriving show]
type token' = [%import: Parser.token [@with Location.t := location']] [@@deriving show]
 *)
let wrap fn =
  fun state oldlexbuf ->
    let token = fn state in
    Sedlexing.fill_lexbuf state.lexbuf oldlexbuf;
    (* Format.eprintf "%a\n%!" pp_token' token; *)
    token

let token' = wrap token

