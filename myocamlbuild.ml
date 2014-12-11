open Ocamlbuild_plugin

let () = dispatch (
  function
  | After_rules ->
    flag ["ocaml"; "compile"; "pp_byte"; "m17n"] &
      S[A"-pp"; A"src/m17n_pp.byte"];
    flag ["ocaml"; "compile"; "pp_native"; "m17n"] &
      S[A"-pp"; A"src/m17n_pp.native"];
    flag ["ocaml"; "ocamldep"; "pp_byte"; "m17n"] &
      S[A"-pp"; A"src/m17n_pp.byte"];
    flag ["ocaml"; "ocamldep"; "pp_native"; "m17n"] &
      S[A"-pp"; A"src/m17n_pp.native"];
    flag ["ocaml"; "pp"; "pp_byte"; "m17n"] &
      A"src/m17n_pp.byte";
    flag ["ocaml"; "pp"; "pp_native"; "m17n"] &
      A"src/m17n_pp.native";

  | _ -> ())
