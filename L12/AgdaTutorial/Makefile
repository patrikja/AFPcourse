test:
	agda Everything.agda

agdadoc:
	agda --html Everything.agda

core: Basics.hcr

Basics.hcr: Basics.hs
	ghc -c -fext-core Basics.hs

clean:
	-rm *.agdai
	-rm *.hi *.o

