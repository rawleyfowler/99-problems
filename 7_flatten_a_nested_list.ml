type 'a node =
  | One of 'a
  | Many of 'a node list;;

(* My solution *)
let rec flatten lst =
  let rec aux acc = function
    | [] -> acc
    | Many a :: t -> aux (acc @ (flatten a)) t
    | One a :: t -> aux (a :: acc) t
  in aux [] lst;;
  
(* Actual solution *)
let flatten list =
  let rec aux acc = function
    | [] -> acc
    | One x :: t -> aux (x :: acc) t
    | Many l :: t -> aux (aux acc l) t 
in List.rev (aux [] list);;

let () = List.iter (Printf.printf "%s, ") (flatten [ One "a" ; Many [ One "b" ; Many [ One "c" ; One "d" ] ; One "e" ] ])