test:
	ocamlbuild -use-ocamlfind test_main.byte && ./test_main.byte

clean:
	ocamlbuild -clean

compile:
	jbuilder build clview.exe
	ocamlbuild -use-ocamlfind state.byte
	ocamlbuild -use-ocamlfind file.byte

ui:
	ocamlbuild -use-ocamlfind controller.byte && ./controller.byte
