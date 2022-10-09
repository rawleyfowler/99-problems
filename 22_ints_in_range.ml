(* My Solution *)
let rec range n k =
  if n > k then
    range k n
  else
    if n = k then
      [k]
    else
      n :: range (succ n) k

(* Actual Solution *)
let range a b =
  let rec aux a b =
    if a > b then [] else a :: aux (a+1) b  in
  if a > b then List.rev (aux b a) else aux a b;;