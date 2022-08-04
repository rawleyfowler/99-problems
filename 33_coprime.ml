(* My solution *)
let coprime x y =
  let s = min x y in
    let rec gcd x' y' n =
      if n > s then true
      else if (y' mod n) = 0 && (x' mod n) = 0 then false
      else gcd x' y' (n + 1)
    in gcd x y 2;;

(* Actual solution where gcd comes from q32 *)
let coprime a b = gcd a b = 1;;