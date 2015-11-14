.phony: all
all :
	bapbuild driver.plugin -pkg bin_prot.syntax -pkg FrontC

test:
	ocaml test.ml
	rm *.dot
	rm *.py

clean :
	bapbuild -clean
	rm *.plugin
