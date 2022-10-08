(* My Solution *)
let drop lst n =
  let v = pred n in
  let rec aux c = function
  | [] -> []
  | h :: t -> 
    if c = 0 then aux v t 
    else h :: aux (pred c) t
  in aux v lst

(* Actual Solution *)
let drop list n =
  let rec aux i = function
    | [] -> []
    | h :: t -> if i = n then aux 1 t else h :: aux (i+1) t  in
  aux 1 list;;