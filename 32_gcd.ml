(* My solution *)
let gcd x y =
  let rec aux x' y' n =
    if x mod n = 0 && y mod n = 0 then n
    else aux x' y' (n - 1)
  in aux x y (min x y);;

let () = print_int (gcd 12 13);;
let () = print_newline ();;
let () = print_int (gcd 4 6);;

(* Actual solution *)
let rec gcd a b =
  if b = 0 then a else gcd b (a mod b);;