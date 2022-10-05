(* My Solution *)
let rec replicate =
  function
  | [] -> fun _ -> []
  | h :: t -> 
    let rec aux acc c n =
      if n > 0 then aux (c :: acc) h (pred n) 
      else acc
    in fun n -> (aux (replicate t n) h n)

(* Actual solution *)
let replicate' list n =
  let rec prepend n acc x =
    if n = 0 then acc else prepend (n-1) (x :: acc) x in
  let rec aux acc = function
    | [] -> acc
    | h :: t -> aux (prepend n acc h) t  in
  (* This could also be written as:
     List.fold_left (prepend n) [] (List.rev list) *)
  aux [] (List.rev list);;