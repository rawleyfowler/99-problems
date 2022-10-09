(* My Solution *)
let rotate lst n =
  let incr_or_decr n =
    if n < 0 then succ n else pred n
  in
  let rec aux v acc = function
  | [] -> acc
  | h :: t as l -> 
    if v = 0 then
      if n < 0 then 
        acc @ (List.rev l) 
      else 
        l @ acc
    else
      aux (incr_or_decr v) (h :: acc) t
  in 
  aux n [] (if n < 0 then List.rev lst else lst)

(* Actual Solution *)
let split list n =
  let rec aux i acc = function
    | [] -> List.rev acc, []
    | h :: t as l -> if i = 0 then List.rev acc, l
                     else aux (i-1) (h :: acc) t  in
  aux n [] list

let rotate list n =
  let len = List.length list in
  (* Compute a rotation value between 0 and len-1 *)
  let n = if len = 0 then 0 else (n mod len + len) mod len in
  if n = 0 then list
  else let a, b = split list n in b @ a;;