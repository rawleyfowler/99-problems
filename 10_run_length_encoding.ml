(* My solution *)
let encode lst = 
  let rec aux curr acc = function
    | [] -> List.rev (curr :: acc)
    | h :: t -> if (fst curr) = h then aux (h, (snd curr) + 1) acc t
                else aux (h, 1) (curr :: acc) t in
  match lst with
  | [] -> []
  | h :: t -> aux (h, 1) [] t;;

let print_int_tuple x = 
  print_string ("(" ^ (string_of_int (fst x)) ^ ", " ^ (string_of_int (snd x)) ^ ")");;

let lst = encode [1; 1; 1; 1; 3; 3; 4; 2; 3; 1; 1; 2; 4; 4; 4; 4; 5];;

let () = List.iter print_int_tuple lst;;

(* Actual solution *)
let encode list =
    let rec aux count acc = function
      | [] -> [] (* Can only be reached if original list is empty *)
      | [x] -> (count + 1, x) :: acc
      | a :: (b :: _ as t) -> if a = b then aux (count + 1) acc t
                              else aux 0 ((count + 1, a) :: acc) t in
    List.rev (aux 0 [] list);;
