type state

val create : Sedlexing_uutf.lexbuf -> state

val skip_sharp_bang : state -> unit
val token : state -> Parser.token

val token' : state -> Lexing.lexbuf -> Parser.token
