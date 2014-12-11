#!/usr/bin/env ocaml
#directory "pkg"
#use "topkg.ml"

let () =
  let oc = open_out "src_test/_tags" in
  output_string oc (if Env.native then "true: pp_native\n" else "true: pp_byte\n");
  output_string oc {|"ru": include|};
  close_out oc

let () =
  Pkg.describe "m17n" ~builder:`OCamlbuild [
    Pkg.lib "pkg/META";
    Pkg.lib ~exts:Exts.interface_opt "src/sedlexing_uutf";
    Pkg.lib ~exts:Exts.interface_opt "src/ulexer";
    Pkg.lib ~exts:Exts.library "src/m17n";
    Pkg.lib ~exts:[".cma"] "src/toploop_m17n";
    Pkg.lib ~cond:(Env.bool "utop") ~exts:[".cma"] "src/utop_m17n";
    Pkg.bin ~auto:true "src/pp_m17n" ~dst:"ocamlm17n";
    Pkg.doc "README.md";
    Pkg.doc "LICENSE.txt";
    Pkg.doc "CHANGELOG.md"; ]
