let rec lex lexbuf =
  match%sedlex lexbuf with
  | "#", Star (Compl "\n") | Chars " \t\r\n" | 0xfeff -> lex lexbuf
  | Plus ('0'..'9' | 'a'..'f' | 'A'..'F') ->
    `Hex (int_of_string ("0x" ^ Sedlexing.Utf8.lexeme lexbuf))
  | ";" -> `Semi
  | "SL" -> `SL
  | "SA" -> `SA
  | "ML" -> `ML
  | "MA" -> `MA
  | eof -> `EOF
  | any ->
    failwith (Printf.sprintf "Unknown character %S at character %d"
                (Sedlexing.Utf8.lexeme lexbuf)
                (Sedlexing.lexeme_start lexbuf))
  | _ -> assert false

exception Parse_error

let parse lexbuf =
  let rec parse_start lst =
    match lex lexbuf with
    | `Hex uchar ->
      begin match lex lexbuf with
      | `Semi -> parse_mapping uchar [] lst
      | _ -> raise Parse_error
      end
    | `EOF -> lst
    | _ -> raise Parse_error
  and parse_mapping uchar mapping lst =
    match lex lexbuf with
    | `Hex mapchar -> parse_mapping uchar (mapchar :: mapping) lst
    | `Semi -> parse_kind uchar mapping lst
    | _ -> raise Parse_error
  and parse_kind uchar mapping lst =
    match lex lexbuf with
    | (`SL | `SA | `ML | `MA) as kind ->
      parse_start ((kind, uchar, mapping) :: lst)
    | _ -> raise Parse_error
  in
  parse_start []

let () =
  let in_chan = open_in Sys.argv.(1) in
  let lexbuf  = Sedlexing.Utf8.from_channel in_chan in
  let data    = parse lexbuf in
  close_in in_chan;

  let out_chan = open_out Sys.argv.(2) in
  let fmt      = Format.formatter_of_out_channel out_chan in
  Format.fprintf fmt "(* Autogenerated. Do not modify. *)@.";
  Format.fprintf fmt "let ma = [|@.@[<hov 2>  ";
  data |>
  List.filter (fun (kind, _, _) -> kind = `MA) |>
  List.sort (fun (_, a, _) (_, b, _) -> compare a b) |>
  List.iter (fun (_, uchar, mapping) ->
    Format.fprintf fmt "0x%04x, [|" uchar;
    mapping |> List.iter (fun uchar' ->
      Format.fprintf fmt "0x%04x; " uchar');
    Format.fprintf fmt "|];@;");
  Format.fprintf fmt "|]@]@.";
  close_out out_chan