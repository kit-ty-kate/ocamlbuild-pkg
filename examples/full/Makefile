all:
	ocamlbuild -use-ocamlfind -plugin-tag "package(ocamlbuild-pkg)"
	cp _build/test-pkg.install .

clean:
	ocamlbuild -clean
	rm -f test-pkg.install

.PHONY: all clean
