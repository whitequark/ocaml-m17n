let () =
  if List.exists ((=) "camlp4o") !Topfind.predicates ||
     List.exists ((=) "camlp4r") !Topfind.predicates then
    print_endline "m17n is incompatible with camlp4!"
  else begin
    Toploop.parse_toplevel_phrase := M17n_util.internationalize Parser.toplevel_phrase;
    Toploop.parse_use_file := M17n_util.internationalize Parser.use_file;
    Toploop.print_out_sig_item := M17n_util.utf8_print_out_sig_item !Toploop.print_out_sig_item;
    Toploop.install_printer Predef.path_string Predef.type_string
      (fun fmt obj -> M17n_util.utf8_print_string fmt (Obj.magic obj));
    prerr_endline "OCaml Multilingualization enabled."
  end
