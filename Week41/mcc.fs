module mcc

open ParseAndComp
[<EntryPoint>]
let main(args) =
    printf "%A" (compileToFile (fromFile args.[0]) args.[1])
    0;;
