type lexbuf

(* The interface used by Sedlex *)
val start : lexbuf -> unit
val next : lexbuf -> Uutf.uchar
val mark : lexbuf -> int -> unit
val backtrack : lexbuf -> int

(** [create ~filename gen] creates a lexing buffer from generator [gen]
    that returns chunks of the input.
    Positions filled with {!fill_lexbuf} will have [filename] as their
    [pos_fname] field. *)
val create : ?filename:string -> string Gen.t -> lexbuf

(** [lexeme lexbuf] returns the list of codepoints comprising
    the current lexeme. *)
val lexeme : lexbuf -> Uutf.uchar list

(** [fill_lexbuf lexbuf oldlexbuf] fills [oldlexbuf.lex_start_p] and [oldlexbuf.lex_curr_p]
    with data from [lexbuf]. *)
val fill_lexbuf : lexbuf -> Lexing.lexbuf -> unit

(** [encode ?normalize lexeme] encodes [lexeme], a list of codepoints,
    as an UTF-8 string, optionally normalizing it as [normalize]. *)
val encode : ?normalize:[< `NFC | `NFD] -> Uutf.uchar list -> string
