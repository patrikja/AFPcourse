MODULE = Properties

run :
	ghc --make ${MODULE}.hs -fhpc
	rm -f ${MODULE}.tix
	./${MODULE}
	hpc markup ${MODULE} --exclude=Main

Main:
	ghc --make Main -fhpc

setup:
	cabal install QuickCheck hpc

clean :
	-rm *.o *.hi
	-rm Main Properties

veryclean : clean
	-rm -r .hpc hpc_index*.html *.hs.html *.tix

