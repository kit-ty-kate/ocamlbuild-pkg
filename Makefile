EXAMPLE_DIRS = \
        full \

all:
	ocaml setup-bootstrap.ml src/ocamlbuild_pkg > myocamlbuild.ml
	ocamlbuild -use-ocamlfind ocamlbuild-pkg

clean: examples-clean
	rm -f myocamlbuild.ml
	ocamlbuild -clean

examples:
	$(foreach d, $(EXAMPLE_DIRS), $(MAKE) -C examples/$(d);)

examples-clean:
	$(foreach d, $(EXAMPLE_DIRS), $(MAKE) -C examples/$(d) clean;)

check:
	dead_code_analyzer.opt --all -S -bind-seq _build/src

.PHONY: all clean examples examples-clean check
