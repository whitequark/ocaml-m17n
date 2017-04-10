open Ocamlbuild_plugin

let () = dispatch (fun phase ->
  Ocamlbuild_cppo.dispatcher phase;
  match phase with
  | After_rules ->
    flag ["ocaml"; "pp"; "pp_byte"; "m17n"] &
      A"src/m17n_pp.byte";
    flag ["ocaml"; "pp"; "pp_native"; "m17n"] &
      A"src/m17n_pp.native";

  | _ -> ())
