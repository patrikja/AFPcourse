# MODULE = Properties
MODULE = Properties_demo

run :
	ghc --make Compiler/${MODULE}.hs -fhpc
	rm -f ${MODULE}.tix
	Compiler/${MODULE}
	hpc markup ${MODULE} --exclude=Main --destdir Compiler

show : run

setup: Parsers.hs Parser1.hs

Parsers.hs:
	ln -s ../lecture4/Parsers.hs

Parser1.hs:
	ln -s ../lecture4/Parser1.hs

# The Compiler example is stored in the Compiler.tgz tar-ball
# It includes its own Makefile
Compiler.tgz: Compiler/*
	tar -zcf Compiler.tgz Compiler

.phony: setup

clean:
	-rm -r .hpc/
	-rm Compiler/Properties_demo Compiler/Properties_demo.tix Properties_demo.tix

veryclean:
	-rm Parsers.hs Parser1.hs
