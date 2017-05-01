ocamlbuild -I src -use-ocamlfind -syntax "camlp4o" 'main.byte'
cp _build/src/main.byte shrdlu
