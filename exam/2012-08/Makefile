default: AFP_exam_20120828.pdf

%.tex: %.lhs
	lhs2TeX --poly $< > $@

# cabal install lhs2TeX

%.pdf: %.tex
	latexmk -pdf $<

AFP_exam_20120828.pdf:: library_documentation.lhs

check:
	runhaskell DSL.hs
	runhaskell Types.hs
	runhaskell Spec.hs

clean:
	-rm -f *.o *.hi
	latexmk -C AFP_exam_20120828.pdf

