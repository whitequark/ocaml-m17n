build:
	cp pkg/META.in pkg/META
	ocaml pkg/build.ml native=true native-dynlink=true utop=true

run: build
	rlwrap ocaml -I _build/src \
		`OCAMLPATH=META ocamlfind query -predicates byte,toploop -a-format findlib m17n`

test: build
	rm -rf _build/src_test
	ocamlbuild -j 0 -use-ocamlfind -classic-display \
		src_test/test_m17n.byte --

clean:
	ocamlbuild -clean

.PHONY: build test clean

VERSION      := $$(opam query --version)
NAME_VERSION := $$(opam query --name-version)
ARCHIVE      := $$(opam query --archive)

release:
	git tag -a v$(VERSION) -m "Version $(VERSION)."
	git push origin v$(VERSION)
	opam publish prepare $(NAME_VERSION) $(ARCHIVE)
	opam publish submit $(NAME_VERSION)
	rm -rf $(NAME_VERSION)

.PHONY: release
