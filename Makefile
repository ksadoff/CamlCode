test:
	ocamlbuild -use-ocamlfind test_main.byte && ./test_main.byte

clean:
	ocamlbuild -clean

compile:
	jbuilder build clview.exe && jbuilder build controller.exe
	ocamlbuild -use-ocamlfind state.byte
	ocamlbuild -use-ocamlfind file.byte

view:
	cd _build && cd default && ./controller.exe
