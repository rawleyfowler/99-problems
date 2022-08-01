(* Non tail recursion *)
let rec length = function
  | [] -> 0
  | _ :: t -> 1 + (length t);;

(* With tail recursion *)
let length_tr lst =
  let rec length_tr' lst' n' =
  match lst' with
  | [] -> n'
  | [x] -> n' + 1
  | _ :: t -> length_tr' t (n' + 1)
  in length_tr' lst 0;;

(* Actual solution *)
let length list =
  let rec aux n = function
    | [] -> n
    | _::t -> aux (n+1) t
  in aux 0 list;;