(* My Solution *)
let rec duplicate = 
  function
  | [] -> []
  | h :: t -> h :: h :: duplicate t

(* Actual Solution *)
let rec duplicate = function
    | [] -> []
    | h :: t -> h :: h :: duplicate t