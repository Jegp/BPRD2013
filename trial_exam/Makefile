ReLex.fs: ReLex.fsl
	mono /home/bladt/build/FSharpPowerPack-4.0.0.0/bin/fslex.exe --unicode ReLex.fsl

ReParse.fs: ReParse.fsy
	mono /home/bladt/build/FSharpPowerPack-4.0.0.0/bin/fsyacc.exe -v --module RePar ReParse.fsy

shell: Absyn.fs ReParse.fs ReLex.fs Parse.fs
	fsharpi --nologo -r ~/build/FSharpPowerPack-4.0.0.0/bin/FSharp.PowerPack.dll Absyn.fs ReParse.fs ReLex.fs Parse.fs
