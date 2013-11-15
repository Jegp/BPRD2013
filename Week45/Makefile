micricc.exe: FSharp.PowerPack.dll Absyn.fs CPar.fs CLex.fs Parse.fs Machine.fs Contcomp.fs MicroCC.fs
	fsharpc -r FSharp.PowerPack.dll Absyn.fs CPar.fs CLex.fs Parse.fs Machine.fs Contcomp.fs MicroCC.fs -o microcc.exe

report: report/week.45.pdf
report/week.45.pdf: report/week.45.tex
	pdflatex -output-directory report/tmp report/week.45.tex 
	pdflatex -output-directory report/tmp report/week.45.tex #yes yes, use texmake or whatevs
	mv report/tmp/week.45.pdf report/week.45.pdf

handin: BPRD-45-SigurtDinesen_JensEgholm.zip
BPRD-45-SigurtDinesen_JensEgholm.zip: report/week.45.pdf Contcomp.fs Absyn.fs CPar.fsy
	zip -u -j $@ report/week.45.pdf Contcomp.fs Absyn.fs CPar.fsy
