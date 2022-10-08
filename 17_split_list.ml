(* My Solution *)
let split lst n =
  let rec aux k acc = function
  | [] -> (acc, [])
  | h :: t ->
    if k = 1 then 
      (h :: acc, t) 
    else 
      aux (pred k) (h :: acc) t
  in 
    if n > List.length lst then
      (lst, [])
    else 
      match aux n [] lst with
      | (h, t) -> (List.rev h, t)

(* Actual solution *)
let split list n =
  let rec aux i acc = function
    | [] -> List.rev acc, []
    | h :: t as l -> if i = 0 then List.rev acc, l
                     else aux (i-1) (h :: acc) t  in
  aux n [] list;;