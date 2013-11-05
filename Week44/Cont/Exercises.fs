
(* Exercise 11.1 *)
let rec lenc xs k =
  match xs with
    | []      -> k 0
    | x :: ys -> lenc ys (fun l -> k (l + 1));;

// i
printf "%A\n" (lenc [2;5;7] id)
printf "%A\n" (lenc [] id)

// ii
printf "%A\n" (lenc [2;3;7] (fun l -> l * 2))

// iii
let rec leni xs i =
  match xs with
    | []      -> i
    | x :: ys -> leni ys (i + 1);;
printf "%A\n" (leni [2;3;1] 0)

(* Exercise 11.2 *)
// i
let rec revc xs k =
  match xs with
    | []      -> k []
    | x :: ys -> revc ys (fun ls -> k (ls @ [x]));;

printf "%A\n" (revc [1;2;3;4] id)

// ii
printf "%A\n" (revc [1;2;3;4] (fun ls -> ls @ ls))

// iii
let rec revi xs ys =
  match xs with
    | []      -> ys
    | x :: zs -> revi zs (x :: ys);;
    
printf "%A\n" (revi [1;2;3] [])

(* Exercise 11.3 *)
let rec prodc xs k =
  match xs with
    | []      -> k 1
    | x :: ys -> prodc ys (fun z -> z * (k x));;

printf "%A\n" (prodc [2;3;6] id)

(* Exercise 11.4 *)
let rec prodo k = function
    | []      -> k 1
    | x :: xs ->
      match x with
        | 0   -> 0
        | _   -> prodo (fun i -> k (i * x)) xs

printf "%A\n" (prodo id [5;3;0])

let rec prodi i = function
  | []      -> i
  | x :: xs ->
    match x with
      | 0 -> 0
      | _ -> prodi (i * x) xs

printf "%A\n" (prodi 1 [3;8;0])

(* Exercises 11.8 *)
run (Every(Write(Prim("+", Prim("*", CstI 2, FromTo(1, 4)), CstI 1))));;
