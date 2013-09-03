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
(*  Exercise 2.1    *)
type expr = 
  | CstI of int
  | Var of string
  | ITE of expr * expr * expr
  | Prim of string * expr * expr
  | Let of (string * expr) list * expr;;

let e1 = CstI 17;;

let e2 = Prim("+", CstI 3, Var "a");;

let e3 = Prim("+", Prim("*", Var "b", CstI 9), Var "a");;

let e4 = Prim("max", CstI 3, Prim("+", CstI 2, Var "x"));;

let e5 = Prim("min", e4, CstI 42);;

let e6 = ITE(CstI 0, CstI 17, CstI 42);;

let e7 = ITE(CstI 1, CstI 17, CstI 42);;

(* Evaluation within an environment *)
(*  Exercise 1.1    *)
let rec eval e (env : (string * int) list) : int =
    match e with
    | CstI i            -> i
    | Var x              -> lookup env x
    | ITE (bool, e1, e2) -> if (eval bool env) = 1 then (eval e1 env) else (eval e2 env)
    | Prim(op, e1, e2) -> 
        let ev1, ev2 = ((eval e1 env), (eval e2 env)) in
        match op with
        | "+"   -> ev1 + ev2
        | "*"   -> ev1 * ev2
        | "-"   -> ev1 - ev2
        | "max" -> max ev1 ev2
        | "min" -> min ev1 ev2
        | "=="  -> if ev1 = ev2 then 1 else 0
        | _     -> failwith "unknown primitive"
    | Let((s, e)::es, fe) -> eval (Let(es, fe)) ((s, eval e env)::env)
    | Let([], fe) -> eval fe env;;

let e1v  = eval e1 env;;
let e2v1 = eval e2 env;;
let e2v2 = eval e2 [("a", 314)];;
let e3v  = eval e3 env;;

let e4v = eval e4 [("x", 3)];;
printfn "%A" e4v //5

let e5v = eval e5 [("x", 3)];;
printfn "%A" e5v //5

let e6v = eval e6 emptyenv;;
printfn "%A" e6v 

let e7v = eval e7 [("x", 3)];;
printfn "%A" e7v 

let e13 = Let ([("a", CstI 2); ("b", CstI 3)], Prim("+", Var "a", Var "b"))
let e13v = eval e13 emptyenv
printfn "e13: %A" e13v

let rec union (xs, ys) = 
    match xs with 
    | []    -> ys
    | x::xr -> if (List.exists (fun elem -> elem = x) ys) then union(xr, ys)
               else x :: union(xr, ys);;

let minus e es =
    List.filter (fun x -> not (e = x)) es;;

let rec freevars e : string list =
    match e with
    | CstI i -> []
    | Var x  -> [x]
    | Let((lhs, rhs)::es, e) -> minus lhs (union (freevars rhs, freevars (Let(es, e))))
    | Let([], e) -> freevars e
    | Prim(ope, e1, e2) -> union (freevars e1, freevars e2)

let rec getindex vs x = 
    match vs with 
    | []    -> failwith "Variable not found"
    | y::yr -> if x=y then 0 else 1 + getindex yr x;;

type texpr =                            (* target expressions *)
  | TCstI of int
  | TVar of int                         (* index into runtime environment *)
  | TLet of texpr * texpr               (* erhs and ebody                 *)
  | TPrim of string * texpr * texpr;;

let rec tcomp (e : expr) (cenv : string list) : texpr =
    match e with
    | CstI i -> TCstI i
    | Var x  -> TVar (getindex cenv x)
    | Let ((lhs, rhs)::es, e) -> 
        let cenv1 = lhs::cenv
        TLet(tcomp rhs cenv1, tcomp (Let(es, e)) cenv1)
    | Let([], e) -> tcomp e cenv
    //| Let(x, erhs, ebody) -> 
    //  let cenv1 = x :: cenv 
    //  TLet(tcomp erhs cenv, tcomp ebody cenv1)
    | Prim(ope, e1, e2) -> TPrim(ope, tcomp e1 cenv, tcomp e2 cenv);;

let e14 = Let([("x1", Prim("+", Var("x2"), CstI 3)); ("x2", CstI 2)], CstI 9)
printfn "e14 (x2): %A" (freevars e14)

let e15 = Let([("x2", CstI 2); ("x1", Prim("+", Var("x2"), CstI 3))], CstI 9)
printfn "e15 (empty): %A" (freevars e15)

let e16 = Let([("x2", CstI 2); ("x1", Prim("+", Var("x2"), CstI 3))], Prim("+", Var "x1", Var "x2"))
printfn "e16 (empty): %A" (freevars e16)

let e17 = Let([("x2", CstI 2); ("x1", Prim("+", Var("x2"), CstI 3))], Prim("+", Var "x1", Var "x3"))
printfn "e17 (x3): %A" (freevars e17)

let e18 = Let([("x2", CstI 2); ("x1", Prim("+", Var("x2"), CstI 3))], Prim("+", Var "x1", Var "x2"))
printfn "e18: %A" (tcomp e18 [])

(*  Exercise 1.2    *)
type aexpr = 
  | CstI of int
  | Var of string
  | Add of aexpr * aexpr
  | Mul of aexpr * aexpr
  | Sub of aexpr * aexpr;;

let e8 = Sub(Var "v", Add(Var "w", Var "z"));;

let e9 = Mul(CstI 2, Sub(Var "v", Add(Var "w", Var "z")));;

let e10 = Add(Var "v", Add(Var "z", Add(Var "y", Var "x")));;

let rec fmt = function
    | CstI i -> string i
    | Var x -> x
    | Add(e1, e2) -> (fmt e1) + " + " + (fmt e2)
    | Sub(e1, e2) -> (fmt e1) + " - " + (fmt e2)
    | Mul(e1, e2) -> (fmt e1) + " * " + (fmt e2);;

let rec simplify = function
    | Mul(x, y) -> 
        let sm = Mul(simplify x, simplify y) in
            match sm with
                | Mul(x, CstI 0) -> CstI 0
                | Mul(CstI 0, x) -> CstI 0
                | Mul(CstI 1, x) -> x
                | Mul(x, CstI 1) -> x
                | a -> a
    | Add(x, y) -> 
        let sa = Add(simplify x, simplify y) in
            match sa with
                | Add(x, CstI 0) -> x
                | Add(CstI 0, x) -> x
                | a -> a
    | Sub(x, y) -> 
        let ss = Sub(simplify x, simplify y) in
            match ss with
                | Sub(x, CstI 0) -> x
                | Sub(x, y) when x=y -> CstI 0
                | a -> a
    | a -> a

let e11 = Add(Var "x", Mul(Var "y", CstI 0))
printfn "%A" (simplify  e11)

let rec diff d = function
    | CstI i -> CstI 0
    | Var(x) when x=d -> CstI 1
    | Var(x) -> CstI 0
    | Mul(x, y) -> Add(Mul(diff d x, y), Mul(x, diff d y))
    | Sub(x, y) -> Sub(diff d x, diff d y)
    | Add(x, y) -> Add(diff d x, diff d y)

let e12 = (diff "x" (Add(CstI 3, Sub(Var "y", Var "x"))))
printfn "%A" e12


// vim: set ts=4 shiftwidth=4 expandtab:
