type 'a rle =
  | One of 'a
  | Many of int * 'a;;

(* My solution *)
let encode lst =
  let rec aux acc = function
    | [] -> List.rev acc
    | h :: t ->
      match acc with
      | [] -> aux (One h :: acc) t
      | h' :: t' -> 
        match h' with
        | One a -> if a = h then aux (Many (2, a) :: t') t 
          else aux (One h :: acc) t
        | Many (a, b) -> if b = h then aux (Many (a + 1, b) :: t') t 
          else aux (One h :: acc) t
        in aux [] lst;;

(* Actual solution *)
let encode_sol l =
    let create_tuple cnt elem =
      if cnt = 1 then One elem
      else Many (cnt, elem) in
    let rec aux count acc = function
      | [] -> []
      | [x] -> (create_tuple (count + 1) x) :: acc
      | hd :: (snd :: _ as tl) ->
          if hd = snd then aux (count + 1) acc tl
          else aux 0 ((create_tuple (count + 1) hd) :: acc) tl in
      List.rev (aux 0 [] l);;

(* Tests *)
let test_data = ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e"] in
let test_solution = encode test_data in
let actual_solution = [Many (4, "a"); One "b"; Many (2, "c"); Many (2, "a"); One "d";
 Many (4, "e")] in
let print_result () = print_endline (if test_solution = actual_solution then "Correct!" else "Not Correct!") in 
print_result ();;