default: AFP_exam_20100824.pdf

%.tex: %.lhs
	lhs2TeX $< > $@

# cabal install lhs2TeX

%.pdf: %.tex
	latexmk -pdf $<

check:
	ghc --make -main-is Problem1.Test.main Problem1.Test && ./Problem1/Test
	ghc --make -c Problem2.hs

clean:
	-rm -f *.o *.hi Problem*/*.o Problem*/*.hi Problem1/Test

