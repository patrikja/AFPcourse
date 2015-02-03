default: AFP_exam_20110316.pdf

%.tex: %.lhs
	lhs2TeX --poly $< > $@

# cabal install lhs2TeX

%.pdf: %.tex
	latexmk -pdf $<

check:
	ghc --make -main-is Problem1.main Problem1 && ./Problem1
	ghc --make -c Problem2.hs
	ghc --make -c Problem3.hs

clean:
	-rm -f *.o *.hi Problem*/*.o Problem*/*.hi Problem1
