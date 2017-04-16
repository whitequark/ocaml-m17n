#!/usr/bin/env ocaml
#directory "pkg"
#use "topkg.ml"

let () =
  let oc = open_out "src_test/_tags" in
  output_string oc (if Env.native then "true: pp_native\n" else "true: pp_byte\n");
  output_string oc {|"ru"|"ja"|"zh": include|};
  close_out oc

let quote_parens s =
  if Sys.win32 then
    s
  else
    "'" ^ s ^ "'"

let ocamlbuild =
  "ocamlbuild -use-ocamlfind -classic-display -plugin-tag " ^
    quote_parens "package(cppo_ocamlbuild)"

let () =
  let exts_lang = [".cmo"; ".cma"; ".cmxs"] in
  Pkg.describe "m17n" ~builder:(`Other (ocamlbuild, "_build")) [
    Pkg.lib "pkg/META";
    Pkg.lib ~exts:Exts.interface_opt "src/m17n_sedlexing";
    Pkg.lib ~exts:Exts.interface_opt "src/m17n_lexer";
    Pkg.lib ~exts:Exts.interface_opt "src/m17n_util";
    Pkg.lib ~exts:Exts.library "src/m17n";
    Pkg.lib ~exts:exts_lang "src_lang/m17n_zh_CN";
    Pkg.lib ~exts:[".cmo"] "src/m17n_toploop";
    Pkg.lib ~cond:(Env.bool "utop") ~exts:[".cmo"] "src/m17n_utop";
    Pkg.bin ~auto:true "src/m17n_pp" ~dst:"ocamlm17n";
    Pkg.doc "README.md";
    Pkg.doc "LICENSE.txt";
    Pkg.doc "CHANGELOG.md"; ]
