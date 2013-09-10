module Lars
printfn "%A" (fromString "2")

let parse s env = Fun.eval (fromString s) env;;

let thousand = "let sum x = if x = 1 then 1 else x + sum (x-1) in sum x end"
printfn "%A" (parse thousand [("x", Fun.Int 1000)])

let unlmtPow = "let pow n = if n = 0 then 1 else 3 * pow (n-1) in pow x end"
printfn "%A" (parse unlmtPow [("x", Fun.Int 8)])

let offlmtPow = "let geomSum n = if n = 0 then 1 else geomSum (n-1) + (pow n) in geomSum x end"
printfn "%A" (parse offlmtPow [("x", Fun.Int 11) ; ("pow", Fun.Closure ("pow", "x", fromString unlmtPow, []))])

let eighth = "let eset t = t in
		let eighth n = if n = 0 then 1 else eighth(n-1) * t in eighth 8 end end"

printfn "%A" (parse eighth [("t", Fun.Int 2)])

let timesSumSquare = "let natRun n = if n = 1 then 1 else eighth (n) + natRun (n-1) in natRun x end"

printfn "%A" (parse timesSumSquare [("eighth", Fun.Closure("eset", "t", fromString eighth, [])) ; ("x", Fun.Int 10)])

//let compoSite = "let eset t = t in
//		   let eighth n = if n = 0 then 1 else eighth(n-1) * t in
//			let natRun n = if n = 1 then 1 else eighth (n) + natRun (n-1) in natRun x end end"
