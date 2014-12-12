#!/usr/bin/env ocaml
#directory "pkg"
#use "topkg.ml"

let () =
  let oc = open_out "src_test/_tags" in
  output_string oc (if Env.native then "true: pp_native\n" else "true: pp_byte\n");
  output_string oc {|"ru"|"ja": include|};
  close_out oc

let () =
  Pkg.describe "m17n" ~builder:`OCamlbuild [
    Pkg.lib "pkg/META";
    Pkg.lib ~exts:Exts.interface_opt "src/m17n_sedlexing";
    Pkg.lib ~exts:Exts.interface_opt "src/m17n_lexer";
    Pkg.lib ~exts:Exts.interface_opt "src/m17n_util";
    Pkg.lib ~exts:Exts.library "src/m17n";
    Pkg.lib ~exts:[".cmo"] "src/m17n_toploop";
    Pkg.lib ~cond:(Env.bool "utop") ~exts:[".cmo"] "src/m17n_utop";
    Pkg.bin ~auto:true "src/m17n_pp" ~dst:"ocamlm17n";
    Pkg.doc "README.md";
    Pkg.doc "LICENSE.txt";
    Pkg.doc "CHANGELOG.md"; ]
