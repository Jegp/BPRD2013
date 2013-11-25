.PHONY: all

all: Regex.pdf

Regex.pdf: tmp/Regex.pdf
	mv $^ $@

tmp/Regex.pdf: Regex.tex 3-2-dfa.png 3-2-nfa.png 3-4.png
	pdflatex -output-directory=tmp Regex.tex
	pdflatex -output-directory=tmp Regex.tex

3-2-dfa.png: 3.2.dfa.dot
	dot $^ -Tpng -o $@

3-2-nfa.png: 3.2.nfa.dot
	dot $^ -Tpng -o $@

3-4.png: 3.4.dot
	dot $^ -Tpng -o $@
