mcc: mcc.fs
	fsharpc --nologo -r ../Week40/MicroC/FSharp.PowerPack.dll Absyn.fs CPar.fs CLex.fs Parse.fs Machine.fs Comp.fs ParseAndComp.fs mcc.fs >/dev/null

CLex.fs: CLex.fsl
	fslex --unicode CLex.fsl

CPar.fs: CPar.fsy
	fsyacc --module CPar CPar.fsy

Machine.class: Machine.java
	javac Machine.java
