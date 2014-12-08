type lexbuf = {
  mutable slex_start_p : Lexing.position;

  (* Current position *)
  mutable slex_curr    : Uutf.uchar Gen.t;
  mutable slex_curr_p  : Lexing.position;

  (* Memorized position *)
  mutable slex_mem     : Uutf.uchar Gen.Restart.t;
  mutable slex_mem_p   : Lexing.position;

  (* Backtracking mark *)
  mutable slex_slot    : int;
}

(* Process a `string gen` and return an `(int, uchar) gen`, iterating
   decoded, normalized Unicode characters, together with their lengths
   in the UTF-8 *byte* representation. *)
let decoder input =
  let uutf = Uutf.decoder ~nln:(`ASCII 0x000A) ~encoding:`UTF_8 `Manual
  and uunf = Uunf.create `NFD
  and pos  = ref 0 in
  let rec gen () =
    (* Do we have any characters queued in normalizer? *)
    match Uunf.add uunf `Await with
    | `Uchar u -> Some (0, u)
    | `Await -> (* No, decode more characters. *)
      match Uutf.decode uutf with
      | (`End | `Uchar _) as result -> (* We advanced in decodeding. *)
        begin match Uunf.add uunf result with
        | `Await ->
          if result = `End then None (* Nothing left to decode. *)
          else gen () (* Normalizer requires more input. *)
        | `Uchar u ->(* Normalizer can return a character. *)
          Some (Uutf.decoder_count uutf - !pos, u)
        end
      | `Await -> (* We exhausted the buffer. *)
        begin match input () with
        | None -> (* We exhausted the input. *)
          Uutf.Manual.src uutf "" 0 0
        | Some chunk -> (* There's some more input. *)
          Uutf.Manual.src uutf chunk 0 (String.length chunk)
        end;
        gen ()
      | `Malformed bytes -> (* The input is malformed. *)
        raise (Invalid_argument "malformed") (* TODO *)
  in
  gen

let create ?(filename="//unknown//") input =
  let pos = Lexing.{
    pos_fname = filename;
    pos_lnum  = 1;
    pos_bol   = 0;
    pos_cnum  = 0; } in
  let restart = Gen.persistent_lazy (decoder input) in
  { slex_start_p = pos;
    slex_curr    = Gen.start restart;
    slex_curr_p  = pos;
    slex_mem     = restart;
    slex_mem_p   = pos;
    slex_slot    = -1; }

let memorize lexbuf =
  let restart = Gen.persistent_lazy lexbuf.slex_curr in
  lexbuf.slex_curr <- Gen.start restart;
  lexbuf.slex_mem_p <- lexbuf.slex_curr_p;
  lexbuf.slex_mem <- restart

let start lexbuf =
  lexbuf.slex_start_p <- lexbuf.slex_curr_p;
  lexbuf.slex_slot <- -1;
  memorize lexbuf

let next lexbuf =
  match lexbuf.slex_curr () with
  | None -> -1
  | Some (pos, uchar) ->
    lexbuf.slex_curr_p.

let mark lexbuf slot =
  lexbuf.slex_slot <- slot;
  memorize lexbuf

let backtrack lexbuf =
  let slot = lexbuf.slex_slot in
  lexbuf.slex_curr <- Gen.start lexbuf.slex_mem;
  lexbuf.slex_curr_p <- lexbuf.slex_mem_p;
  slot

let fill_lexbuf lexbuf oldlexbuf =
  oldlexbuf.lex_start_p <- lexbuf.slex_start_p;
  oldlexbuf.lex_curr_p <- lexbuf.slex_curr_p
