type state

val create : Sedlexing_uutf.lexbuf -> state

val token : state -> Parser.token

val token' : state -> Lexing.lexbuf -> Parser.token
