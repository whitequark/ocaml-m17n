(** [gen_of_lexbuf lexbuf] returns a generator that uses [lexbuf]'s
    refill function and returns a sequence of [chunk, start, len]
    tuples containing the data extracted from [lexbuf]. *)
val gen_of_lexbuf : Lexing.lexbuf -> (bytes * int * int) Gen.t

(** [internationalize parse_fn] accepts an ocamlyacc parse function
    [parse_fn] and returns an adjusted function that uses {!M17n_lexer}.
    The input filename is extracted from [Toploop.input_name] (an alias
    to [Location.input_name]).
    When the input filename is ["//toplevel//"], toplevel-specific error
    handling is enabled. *)
val internationalize : ((Lexing.lexbuf -> Parser.token) -> Lexing.lexbuf -> 'a) ->
                       (Lexing.lexbuf -> 'a)
