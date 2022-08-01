(* My Solution *)
let rec last_two = function
  | [] -> None
  | [x] -> None
  | [x; y] -> Some (x, y)
  | (_ :: t) -> last_two t;;

(* Actual solution *)
let rec last_two = function
  | [] | [_] -> None
  | [x;y] -> Some (x,y)
  | _::t -> last_two t;;