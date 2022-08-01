(* My solution *)
let reverse lst =
  let rec aux lst' acc' =
    match lst' with
    | [] -> acc'
    | (h :: t) -> aux t (h :: acc')
  in aux lst [];;

(* Actual Solution *)
let rev list =
  let rec aux acc = function
    | [] -> acc
    | h::t -> aux (h::acc) t in
  aux [] list;;