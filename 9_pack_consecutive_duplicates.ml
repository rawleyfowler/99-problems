(* My solution *)
let pack lst =
  let rec aux acc sub_acc = function
    | h :: (a :: _ as t) -> 
      if h = a then aux acc (h :: sub_acc) t 
      else aux (sub_acc :: acc) [] t
    | [x] -> (x :: sub_acc) :: acc
    | [] -> []
    in aux [] [] lst;;

(* Actual solution *)
let pack list =
  let rec aux current acc = function
    | [] -> []    (* Can only be reached if original list is empty *)
    | [x] -> (x :: current) :: acc
    | a :: (b :: _ as t) ->
      if a = b then aux (a :: current) acc t
      else aux [] ((a :: current) :: acc) t 
    in List.rev (aux [] [] list);;