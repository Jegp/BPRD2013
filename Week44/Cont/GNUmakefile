report: report/week.44.pdf
report/week.44.pdf: report/week.44.tex
	pdflatex -output-directory report/tmp report/week.44.tex 
	pdflatex -output-directory report/tmp report/week.44.tex #yes yes, use texmake or whatevs
	mv report/tmp/week.44.pdf report/week.44.pdf

handin: BPRD-44-SigurtDinesen_JensEgholm.zip:
BPRD-44-SigurtDinesen_JensEgholm.zip: report/week.44.pdf Exercises.fs Icon.fs
	zip -u -j report/week.44.pdf Exercises.fs Icon.fs
