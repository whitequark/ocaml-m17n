type lexbuf = {
  mutable slex_start_p    : Lexing.position;
  mutable slex_curr       : (int * Uutf.uchar) Gen.clonable;
  mutable slex_curr_g     : (int * Uutf.uchar) Gen.t;
  mutable slex_curr_p     : Lexing.position;
  mutable slex_lexeme     : int list;

  mutable slex_slot       : int;
  mutable slex_mem        : (int * Uutf.uchar) Gen.clonable;
  mutable slex_mem_p      : Lexing.position;
  mutable slex_mem_lexeme : int list;
}

let uchar_len u =
  let buf  = Bytes.create 5 in
  let uutf = Uutf.encoder `UTF_8 `Manual in
  Uutf.Manual.dst uutf (Bytes.unsafe_to_string buf) 0 5;
  assert (`Ok = Uutf.encode uutf (`Uchar u));
  Bytes.length buf - (Uutf.Manual.dst_rem uutf)

(* Process a `string gen` and return an `(int, uchar) gen`, iterating
   decoded Unicode characters, together with their lengths in
   the UTF-8 *byte* representation. *)
let decoder kind input =
  let uutf = Uutf.decoder ~nln:(`ASCII 0x000A) ~encoding:`UTF_8 `Manual in
  let rec gen () =
    match Uutf.decode uutf with
    | `End -> None
    | `Uchar u ->
      if kind = `Batch then
        Some (1, u)
      else
        Some (uchar_len u, u) (* TODO *)
    | `Await -> (* We exhausted the buffer. *)
      begin match input () with
      | None -> (* We exhausted the input. *)
        Uutf.Manual.src uutf "" 0 0
      | Some (chunk, start, len) -> (* There's some more input. *)
        Uutf.Manual.src uutf (Bytes.unsafe_to_string chunk) start len
      end;
      gen ()
    | `Malformed bytes -> (* The input is malformed. *)
      (* Return U+FFFD, it will be handled by the lexer. *)
      if kind = `Batch then
        Some (1, Uutf.u_rep)
      else
        Some (String.length bytes, Uutf.u_rep)
  in
  gen

let create ?(kind=`Batch) ?(filename="//unknown//") input =
  let pos = Lexing.{
    pos_fname = filename;
    pos_lnum  = 1;
    pos_bol   = 0;
    pos_cnum  = 0; } in
  let gen =
    match kind with
    | `Batch -> Gen.MList.(to_clonable (of_gen_lazy (decoder kind input)))
    | `Toplevel -> assert false
  in
  { slex_start_p    = pos;
    slex_curr       = gen;
    slex_curr_g     = gen#next;
    slex_curr_p     = pos;
    slex_lexeme     = [];
    slex_slot       = -1;
    slex_mem        = gen#clone;
    slex_mem_p      = pos;
    slex_mem_lexeme = []; }

let memorize lexbuf =
  lexbuf.slex_mem_lexeme <- lexbuf.slex_lexeme;
  lexbuf.slex_mem_p <- lexbuf.slex_curr_p;
  lexbuf.slex_mem <- lexbuf.slex_curr#clone

let start lexbuf =
  lexbuf.slex_start_p <- lexbuf.slex_curr_p;
  lexbuf.slex_lexeme <- [];
  lexbuf.slex_slot <- -1;
  memorize lexbuf

let next lexbuf =
  let open Lexing in
  match lexbuf.slex_curr_g () with
  | None -> -1
  | Some (len, uchar) ->
    let pos = lexbuf.slex_curr_p in
    if uchar = 0x000A then
      lexbuf.slex_curr_p <- { pos with
        pos_lnum = pos.pos_lnum + 1;
        pos_cnum = pos.pos_cnum + len;
        pos_bol  = pos.pos_cnum + len; }
    else
      lexbuf.slex_curr_p <- { pos with
        pos_cnum = pos.pos_cnum + len; };
    lexbuf.slex_lexeme <- uchar :: lexbuf.slex_lexeme;
    uchar

let mark lexbuf slot =
  lexbuf.slex_slot <- slot;
  memorize lexbuf

let backtrack lexbuf =
  let slot = lexbuf.slex_slot in
  lexbuf.slex_curr <- lexbuf.slex_mem#clone;
  lexbuf.slex_curr_g <- lexbuf.slex_curr#next;
  lexbuf.slex_curr_p <- lexbuf.slex_mem_p;
  lexbuf.slex_lexeme <- lexbuf.slex_mem_lexeme;
  slot

let lexeme lexbuf =
  List.rev lexbuf.slex_lexeme

let lexeme_char n lexbuf =
  List.nth (lexeme lexbuf) n

let sub_lexeme (lft, rgt) lexbuf =
  let rec drop i =
    function
    | x::lst when i > 0 -> drop (i-1) lst
    | [] when i > 0 -> assert false
    | lst -> lst
  in
  let map i = if i >= 0 then i else -(i + 1) in
  lexbuf.slex_lexeme |> drop (map rgt) |>
  List.rev |> drop (map lft)

let fill_lexbuf lexbuf oldlexbuf =
  let open Lexing in
  oldlexbuf.lex_start_p <- lexbuf.slex_start_p;
  oldlexbuf.lex_curr_p <- lexbuf.slex_curr_p

let uunf_normalize form uchars =
  let buf  = Buffer.create (List.length uchars) in
  let uunf = Uunf.create form in
  let rec add uchar =
    match Uunf.add uunf uchar with
    | `Uchar u -> Uutf.Buffer.add_utf_8 buf u; add `Await
    | `Await -> ()
  in
  List.iter (fun uchar -> add (`Uchar uchar)) uchars;
  add `End;
  Buffer.contents buf

let encode ?normalize uchars =
  match normalize with
  | None ->
    let buf = Buffer.create (List.length uchars) in
    List.iter (Uutf.Buffer.add_utf_8 buf) uchars;
    Buffer.contents buf
  | Some form ->
    uunf_normalize form uchars

let utf8_lexeme ?normalize lexbuf =
  encode ?normalize (lexeme lexbuf)

let utf8_sub_lexeme ?normalize range lexbuf =
  encode ?normalize (sub_lexeme range lexbuf)

let expand_token lexbuf f =
  let start_p = lexbuf.slex_start_p in
  let result = f () in
  lexbuf.slex_start_p <- start_p;
  result

let location lexbuf =
  Location.{
    loc_ghost = false;
    loc_start = lexbuf.slex_start_p;
    loc_end   = lexbuf.slex_curr_p; }

let set_position lexbuf file line =
  let open Lexing in
  lexbuf.slex_start_p <- { lexbuf.slex_start_p with
    pos_fname = file;
    pos_lnum  = line; }

let rec gen_prepend elem rest =
  object
    val mutable state = `Yield
    method next () =
      match state with
      | `Yield -> state <- `Fwd; Some elem
      | `Fwd -> rest#next ()
    method clone =
      match state with
      | `Yield -> gen_prepend elem rest#clone
      | `Fwd -> rest#clone
  end

let unshift lexbuf =
  let open Lexing in
  match lexbuf.slex_lexeme with
  | [] -> assert false
  | uchar :: lexeme ->
    assert (uchar <> 0x000A && uchar < 0x0100);
    lexbuf.slex_curr <- gen_prepend (1, uchar) lexbuf.slex_curr;
    lexbuf.slex_curr_g <- lexbuf.slex_curr#next;
    lexbuf.slex_curr_p <- { lexbuf.slex_curr_p with
      pos_cnum = lexbuf.slex_curr_p.pos_cnum - 1; };
    lexbuf.slex_lexeme <- lexeme
