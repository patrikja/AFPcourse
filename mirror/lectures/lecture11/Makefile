runMiddle :
	ghc --make Middle -fhpc \
            -main-is Middle.main
	rm -f Middle.tix
	./Middle
	hpc markup Middle

runArray :
	ghc --make Array.Properties -fhpc \
            -main-is Array.Properties.main 
	rm -f Properties.tix
	Array/Properties
	hpc markup Properties \
            --exclude=Array.ShowInstances \
            --exclude=Array.EqInstances


clean:
	-rm hpc_index*.html
	-rm *.hs.html
	-rm Middle
	-rm Middle.tix
	-rm Array/Properties
	-rm Properties.tix
#	-rm -r .hpc
