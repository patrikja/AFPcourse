hpcMiddle :
	ghc --make Middle -fhpc \
            -main-is Middle.main
	rm -f Middle.tix
	./Middle
	hpc markup Middle

clean:
	-rm hpc_index*.html
	-rm *.hs.html
	-rm Middle
	-rm Middle.tix
