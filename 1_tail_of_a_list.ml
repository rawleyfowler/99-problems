(* My Solution *)
let rec last lst =
  match lst with
  | [] -> None
  | _ -> let rec last' (h :: t) = 
    match t with
    | [] -> h
    | _ -> last' t
  in Some (last' lst);;

(* Actual solution*)
let rec last = function
  | [] -> None
  | [x] -> Some x
  | _ :: t -> last t;;