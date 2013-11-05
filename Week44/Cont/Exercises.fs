
(* Exercise 11.1 *)
let rec lenc xs k =
  match xs with
    | []      -> k 0
    | x :: ys -> lenc ys (fun l -> k (l + 1))

// i
printf "%A\n" (lenc [2;5;7] id)
printf "%A\n" (lenc [] id)

// ii
printf "%A\n" (lenc [2;3;7] (fun l -> l * 2))

(* Exercise 11.2 *)

