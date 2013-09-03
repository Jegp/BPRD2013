(* Programming language concepts for software developers, 2010-08-28 *)

(* Evaluating simple expressions with variables *)

module Intro2

(* Association lists map object language variables to their values *)

let env = [("a", 3); ("c", 78); ("baf", 666); ("b", 111)];;

let emptyenv = []; (* the empty environment *)

let rec lookup env x =
    match env with 
    | []        -> failwith (x + " not found")
    | (y, v)::r -> if x=y then v else lookup r x;;

let cvalue = lookup env "c";;


(* Object language expressions with variables *)

type expr = 
  | CstI of int
  | If of expr * expr * expr
  | Var of string
  | Let of (string * expr) list * expr
  | Prim of string * expr * expr;;

let e1 = CstI 17;;

let e2 = Prim("+", CstI 3, Var "a");;

let e3 = Prim("+", Prim("*", Var "b", CstI 9), Var "a");;


(* Evaluation within an environment *)

let rec eval e (env : (string * int) list) : int =
    match e with
    | CstI i              -> i
    | Var x               -> lookup env x 
    | If(e1, e2, e3)      -> if (eval e1 env) = 1 then (eval e2 env) else (eval e3 env)
    | Let(lets, e)        -> eval e (List.fold (fun acc (name, expr) -> (name, (eval expr acc)) :: acc) env lets)
    | Prim(ope, e1, e2)   ->
      let i1 = eval e1 env
      let i2 = eval e2 env
      match ope with
        | "+"   -> i1 + i2
        | "-"   -> i1 - i2
        | "*"   -> i1 * i2
        | "min" -> min i1 i2
        | "max" -> max i1 i2
        | "=="  -> if i1 = i2 then 1 else 0
        | _     -> failwith "unknown primitive";;

let e1v  = eval e1 env;;
let e2v1 = eval e2 env;;
let e2v2 = eval e2 [("a", 314)];;
let e3v  = eval e3 env;;

let e4 = eval (Prim("max", CstI 5, Var "x")) [("x", 6)];;
let e5 = eval (Prim("==", Var "x", Prim("min", CstI 5, Var "y"))) [("y", 3) ; ("x", 9)];;
let e6 = eval (If(CstI 0, CstI 2, CstI 3)) [];;

// 2.1
let e16 = eval (Let([("x1", CstI 10) ; ("x2", Var "x1")], Var "x2")) []

type aexpr =
  | CstI of int
  | Var of string
  | Add of aexpr * aexpr
  | Mul of aexpr * aexpr
  | Sub of aexpr * aexpr;;;

let e7 = Sub(Var "v", Add(Var "w", Var "z"));;
let e8 = Sub(CstI 2, Sub(Var "v", Add(Var "w", Var "z")));;
let e9 = Add(Var "v", Add(Var "z", Add(Var "y", Var "x")));;

let rec fmt (e : aexpr) : string =
  match e with
    | CstI(x)   -> string(x)
    | Var(x)    -> x
    | Add(x, y) -> "(" + fmt x + " + " + fmt y + ") "
    | Mul(x, y) -> "(" + fmt x + " * " + fmt y + ") "
    | Sub(x, y) -> "(" + fmt x + " - " + fmt y + ") ";;

let e10 = fmt (Add (CstI 2, CstI 6));;

let rec simplify (e : aexpr) : aexpr =
  match e with
    | Add(x, y)      ->
      let i1 = simplify x
      let i2 = simplify y
      match (i1, i2) with
        | (CstI 0, z) -> z
        | (z, CstI 0) -> z
        | _ -> Add(i1, i2)
    | Sub(x, y)      ->
      let i1 = simplify x
      let i2 = simplify y
      match (i1, i2) with
        | (z, CstI 0) -> z
        | _ -> if i1 = i2 then CstI 0 else Sub(i1, i2)
    | Mul(x, y)      ->
      let i1 = simplify x
      let i2 = simplify y
      match (i1, i2) with
        | (CstI 0, y) -> CstI 0
        | (CstI 1, y) -> y
        | (x, CstI 0) -> CstI 0
        | (x, CstI 1) -> x
        | _ -> Mul(i1, i2)
    | a -> a;;

let e11 = simplify (Mul(CstI 0, Var "x"));;
let e12 = simplify (Sub(Var "y", Sub(CstI 3, Mul(CstI 0, CstI 10))));;
let e13 = simplify (Mul(Add(CstI 1, CstI 0), Add(Var "x", CstI 0)));;

let rec differentiate d = function
  | CstI(x)           -> CstI(0)
  | Var(x) when x = d -> CstI(1)
  | Var(x)            -> CstI(0)
  | Mul(x, y) -> Add(Mul(differentiate d x, y), Mul(x, differentiate d y))
  | Add(x, y) -> Add(differentiate d x, differentiate d y)
  | Sub(x, y) -> Sub(differentiate d x, differentiate d y);;

let e14 = differentiate "x" (CstI 10);;
let e15 = differentiate "x" (Mul(CstI 5, Add(Var "x", CstI 6)));;


printfn "%A" e16;;

