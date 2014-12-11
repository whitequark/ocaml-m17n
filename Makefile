build:
	cp pkg/META.in pkg/META
	ocaml pkg/build.ml native=true native-dynlink=true utop=true

run: build
	rlwrap ocaml \
		$(shell ocamlfind query -predicates byte,toploop -r -a-format \
		                        findlib compiler-libs.common gen uutf uunf uucp) \
		_build/src/m17n.cma _build/src/m17n_toploop.cmo

run_utop: build
	utop \
		$(shell ocamlfind query -predicates byte,toploop -r -a-format \
		                        compiler-libs.common gen uutf uunf uucp) \
		_build/src/m17n.cma _build/src/m17n_utop.cmo

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
