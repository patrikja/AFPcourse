MODULE = Properties
# MODULE = Properties_demo

Compiler/hpc_index.html : Compiler/${MODULE}.hs
	ghc --make Compiler/${MODULE}.hs -main-is Compiler.${MODULE}.main -fhpc
	rm -f ${MODULE}.tix
	Compiler/${MODULE}
	hpc markup ${MODULE} --exclude=Main --destdir Compiler

show : Compiler/hpc_index.html
	firefox Compiler/hpc_index.html

clean:
	-rm -r .hpc/
	-rm Compiler/${MODULE} Compiler/${MODULE}.tix ${MODULE}.tix
	-rm Compiler/*.html

