(* My solution *)
let coprime x y =
  let s = min x y in
    let rec gcd x' y' n =
      if n > s then true
      else if (y' mod n) = 0 && (x' mod n) = 0 then false
      else gcd x' y' (n + 1)
    in gcd x y 2;;

let phi n =
  let rec aux n' =
    match n' with
    | 0 -> 0
    | 1 -> 1
    | _ -> 
      let next = aux (n' - 1) in
        if coprime n' n then 1 + next
        else next
  in aux (n - 1);;

let () = print_int (phi 10);;
let () = print_newline ();;
let () = print_int (phi 13);;

(* Actual solution *)
let phi n =
  let rec count_coprime acc d =
    if d < n then
      count_coprime (if coprime n d then acc + 1 else acc) (d + 1)
    else acc
  in
  if n = 1 then 1 else count_coprime 0 1;;