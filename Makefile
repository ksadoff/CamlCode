test:
	ocamlbuild -use-ocamlfind test_main.byte && ./test_main.byte

clean:
	ocamlbuild -clean

compile:
	jbuilder build clview.exe
	ocamlbuild -use-ocamlfind state.byte
	ocamlbuild -use-ocamlfind file.byte

view:
	cd _build && cd default && ./clview.exe
