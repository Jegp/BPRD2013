mcc.exe: mcc.fs CLex.fs CPar.fs Comp.fs
	fsharpc --nologo -r FSharp.PowerPack.dll Absyn.fs CPar.fs CLex.fs Parse.fs Machine.fs Comp.fs ParseAndComp.fs mcc.fs > /dev/null

report: report/tmp/week.41.pdf
report/tmp/week.41.pdf: report/week.41.tex
	pdflatex --output-directory report/tmp report/week.41.tex

CLex.fs: CLex.fsl
	/home/bladt/build/FSharpPowerPack-4.0.0.0/bin/fslex.exe --unicode CLex.fsl

CPar.fs: CPar.fsy
	/home/bladt/build/FSharpPowerPack-4.0.0.0/bin/fsyacc.exe --module CPar CPar.fsy

Machine.class: Machine.java
	javac Machine.java

clean:
	rm CLex.fs CPar.fs Machine.class Machinetrace.class mcc.exe
