(** [gen_of_lexbuf lexbuf] returns a generator that uses [lexbuf]'s
    refill function and returns a sequence of [chunk, start, len]
    tuples containing the data extracted from [lexbuf]. *)
val gen_of_lexbuf : Lexing.lexbuf -> (bytes * int * int) Gen.t

(** [utf8_print_string fmt s] either prints a valid UTF-8 string [s] without
    control characters into [fmt] quoted and unescaped, or if it is not,
    prints it into [fmt] with format ["%S"]. *)
val utf8_print_string : Format.formatter -> string -> unit

(** {2 Parse wrappers} *)

(** [internationalize parse_fn] accepts an ocamlyacc parse function
    [parse_fn] and returns an adjusted function that uses {!M17n_lexer}.
    The input filename is extracted from [Toploop.input_name] (an alias
    to [Location.input_name]).
    When the input filename is ["//toplevel//"], toplevel-specific error
    handling is enabled. *)
val internationalize : ((Lexing.lexbuf -> Parser.token) -> Lexing.lexbuf -> 'a) ->
                       (Lexing.lexbuf -> 'a)

(** {2 Oprint wrappers} *)

(** [utf8_parenthesized_ident] is an UTF-8-aware version of
    [Oprint.parenthesized_ident]. *)
val utf8_parenthesized_ident : string -> bool

(** [utf8_value_ident] is an UTF-8-aware version of
    [Oprint.value_ident]. *)
val utf8_value_ident : Format.formatter -> string -> unit

(** [utf8_print_out_sig_item next] is an UTF-8-aware version of
    [Oprint.print_out_sig_item].
    It adjusts the printing of [val] signature items and delegates
    the rest to [next]. *)
val utf8_print_out_sig_item :
    (Format.formatter -> Outcometree.out_sig_item -> unit) ->
    Format.formatter -> Outcometree.out_sig_item -> unit
