let () =
  if List.exists ((=) "camlp4o") !Topfind.predicates ||
     List.exists ((=) "camlp4r") !Topfind.predicates then
    print_endline "m17n is incompatible with camlp4!"
  else begin
    Toploop.parse_toplevel_phrase := M17n_wrap.internationalize Parser.toplevel_phrase;
    Toploop.parse_use_file := M17n_wrap.internationalize Parser.use_file;
    prerr_endline "OCaml Multilingualization enabled."
  end
