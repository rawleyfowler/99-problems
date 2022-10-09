(* My Solution *)
let rec insert_at el n = function
  | [] -> []
  | h :: t as k ->
    if n = 0 then
      el :: k
    else
      h :: insert_at el (pred n) t

(* Actual Solution *)
let rec insert_at x n = function
    | [] -> [x]
    | h :: t as l -> if n = 0 then x :: l else h :: insert_at x (n-1) t;;