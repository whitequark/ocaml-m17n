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

exception Error of error * Location.t

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

let () =
  Location.register_error_of_exn
    (function
    | Error (err, loc) ->
      Some (Location.error_of_printer loc report_error err)
    | _ -> None)

let uppercase = [%sedlex.regexp? lu ]
let lowercase = [%sedlex.regexp? ll | lm | lo | lt | '_' ]
let identchar = [%sedlex.regexp? uppercase | lowercase | nd | nl | no | "'" ]
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

type state = {
  lexbuf : Sedlexing.lexbuf;
}

let create lexbuf =
  { lexbuf }

let get_label_name lexbuf =
  let name = Sedlexing.utf8_sub_lexeme ~normalize:`NFC (1, -2) lexbuf in
  if Hashtbl.mem keyword_table name then
    raise (Lexer.Error (Lexer.Keyword_as_label name, Sedlexing.location lexbuf));
  name

let convert_int_literal s =
  - int_of_string ("-" ^ s)
let convert_int32_literal s =
  Int32.neg (Int32.of_string ("-" ^ String.sub s 0 (String.length s - 1)))
let convert_int64_literal s =
  Int64.neg (Int64.of_string ("-" ^ String.sub s 0 (String.length s - 1)))
let convert_nativeint_literal s =
  Nativeint.neg (Nativeint.of_string ("-" ^ String.sub s 0 (String.length s - 1)))

let rec token ({ lexbuf } as state) =
  match%sedlex lexbuf with
  | "\\\n" ->
    raise (Error (Illegal_character (List.hd (Sedlexing.lexeme lexbuf)),
                  Sedlexing.location lexbuf))
  | '\n' -> token state
  | Plus (zs | zl | zp) -> token state
  | '_' -> UNDERSCORE
  | '~' -> TILDE
  | '~', lowercase, Star identchar, ':' ->
    LABEL (get_label_name lexbuf)
  | '?' -> QUESTION
  | '?', lowercase, Star identchar, ':' ->
    OPTLABEL (get_label_name lexbuf)
  | lowercase, Star identchar ->
    let str = Sedlexing.utf8_lexeme ~normalize:`NFC lexbuf in
    (try Hashtbl.find keyword_table str
     with Not_found -> LIDENT str)
  | uppercase, Star identchar ->
    UIDENT (Sedlexing.utf8_lexeme ~normalize:`NFC lexbuf)
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
  (*| "\""
      { reset_string_buffer();
        is_in_string := true;
        let string_start = lexbuf.lex_start_p in
        string_start_loc := Location.curr lexbuf;
        string lexbuf;
        is_in_string := false;
        lexbuf.lex_start_p <- string_start;
        STRING (get_stored_string(), None) }
  | "{" lowercase* "|"
      { reset_string_buffer();
        let delim = Lexing.lexeme lexbuf in
        let delim = String.sub delim 1 (String.length delim - 2) in
        is_in_string := true;
        let string_start = lexbuf.lex_start_p in
        string_start_loc := Location.curr lexbuf;
        quoted_string delim lexbuf;
        is_in_string := false;
        lexbuf.lex_start_p <- string_start;
        STRING (get_stored_string(), Some delim) }
  | "'" newline "'"
      { update_loc lexbuf None 1 false 1;
        CHAR (Lexing.lexeme_char lexbuf 1) }
  | "'" [^ '\\' '\'' '\010' '\013'] "'"
      { CHAR(Lexing.lexeme_char lexbuf 1) }
  | "'\\" ['\\' '\'' '"' 'n' 't' 'b' 'r' ' '] "'"
      { CHAR(char_for_backslash (Lexing.lexeme_char lexbuf 2)) }
  | "'\\" ['0'-'9'] ['0'-'9'] ['0'-'9'] "'"
      { CHAR(char_for_decimal_code lexbuf 2) }
  | "'\\" 'x' ['0'-'9' 'a'-'f' 'A'-'F'] ['0'-'9' 'a'-'f' 'A'-'F'] "'"
      { CHAR(char_for_hexadecimal_code lexbuf 3) }
  | "'\\" _
      { let l = Lexing.lexeme lexbuf in
        let esc = String.sub l 1 (String.length l - 1) in
        raise (Error(Illegal_escape esc, Location.curr lexbuf))
      }
  | "( *"
      { let start_loc = Location.curr lexbuf  in
        comment_start_loc := [start_loc];
        reset_string_buffer ();
        let end_loc = comment lexbuf in
        let s = get_stored_string () in
        reset_string_buffer ();
        COMMENT (s, { start_loc with
                      Location.loc_end = end_loc.Location.loc_end })
      }
  | "( * )"
      { let loc = Location.curr lexbuf  in
        if !print_warnings then
          Location.prerr_warning loc Warnings.Comment_start;
        comment_start_loc := [loc];
        reset_string_buffer ();
        let end_loc = comment lexbuf in
        let s = get_stored_string () in
        reset_string_buffer ();
        COMMENT (s, { loc with Location.loc_end = end_loc.Location.loc_end })
      }
  | "* )"
      { let loc = Location.curr lexbuf in
        Location.prerr_warning loc Warnings.Comment_not_end;
        lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_curr_pos - 1;
        let curpos = lexbuf.lex_curr_p in
        lexbuf.lex_curr_p <- { curpos with pos_cnum = curpos.pos_cnum - 1 };
        STAR
      }
  | "#" [' ' '\t']* (['0'-'9']+ as num) [' ' '\t']*
        ("\"" ([^ '\010' '\013' '"' ] * as name) "\"")?
        [^ '\010' '\013'] * newline
      { update_loc lexbuf name (int_of_string num) true 0;
        token lexbuf
      }*)
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

type position' = [%import: Lexing.position] [@@deriving show]
type location' = [%import: Location.t [@with Lexing.position := position']] [@@deriving show]
type token' = [%import: Parser.token [@with Location.t := location']] [@@deriving show]

let wrap fn =
  fun state oldlexbuf ->
    let token = fn state in
    Sedlexing_uutf.fill_lexbuf state.lexbuf oldlexbuf;
    (* prerr_endline (show_token' token); *)
    token

let token' = wrap token

