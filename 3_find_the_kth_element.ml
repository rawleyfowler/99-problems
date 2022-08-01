(* My Solution *)
let rec at n (h :: t) =
  match t with
  | [] -> None
  | _ -> if n = 0 then Some h else at (n - 1) t;;

(* Actual solution *)
let rec at k = function
  | [] -> None
  | h :: t -> if k = 1 then Some h else at (k - 1) t;;