open Ocamlbuild_plugin

let () = dispatch (
  function
  | After_rules ->
    flag ["ocaml"; "compile"; "ppx_byte"; "m17n"] &
      S[A"-pp"; A"src/pp_m17n.byte"];
    flag ["ocaml"; "compile"; "ppx_native"; "m17n"] &
      S[A"-pp"; A"src/pp_m17n.native"];

  | _ -> ())
