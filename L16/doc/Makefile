default: AFP_learning_outcomes.pdf

AFP_learning_outcomes.pdf: AFP_learning_outcomes.tex overview.tex coverage.tex

# %.html: %.markdown
# 	pandoc --css=../../css/progs.css $< -o $@

%.tex: %.markdown
	pandoc $< -o $@

%.pdf: %.tex
	latexmk -pdf $<

clean:
	-rm overview.html overview.tex coverage.html coverage.tex
