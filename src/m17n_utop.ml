let () =
  if List.exists ((=) "camlp4o") !Topfind.predicates ||
     List.exists ((=) "camlp4r") !Topfind.predicates then
    print_endline "m17n is incompatible with camlp4!"
  else begin
    UTop.parse_toplevel_phrase :=
      UTop.parse_default (M17n_wrap.internationalize Parser.toplevel_phrase);
    UTop.parse_use_file :=
      UTop.parse_default (M17n_wrap.internationalize Parser.use_file);
    (* As of 1.16, utop does not actually invoke UTop.parse_use_file. *)
    Toploop.parse_use_file := M17n_wrap.internationalize Parser.use_file;
  end
