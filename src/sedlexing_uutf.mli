type lexbuf

(* Required by sedlex *)
val start : lexbuf -> unit
val next : lexbuf -> int
val mark : lexbuf -> int -> unit
val backtrack : lexbuf -> int

(** [create ~filename gen] creates a lexing buffer from generator [gen]
    that returns chunks of the input.
    Positions filled with {!fill_lexbuf} will have [filename] as their
    [pos_fname] field. *)
val create : ?filename:string -> string Gen.t -> lexbuf

(** [fill_lexbuf sedlexbuf lexbuf] fills [lexbuf.lex_start_p] and [lexbuf.lex_curr_p]
    with data from [sedlexbuf]. *)
val fill_lexbuf : lexbuf -> Lexing.lexbuf -> unit
