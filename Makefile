all:
	ocamlbuild -pkg graphics -pkg unix diagram.native test_solution.native
