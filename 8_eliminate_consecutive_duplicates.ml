(* My solution *)
let compress lst =
  let rec aux c acc = function
  | [] -> acc
  | (h :: t) ->
    match c with
    | None -> aux (Some h) (h :: acc) t
    | Some v -> if v = h 
      then aux c acc t
      else aux (Some h) (h :: acc) t
    in aux None [] lst;;

(* My solution take 2 *)
let rec compress = function
  | h :: (a :: t) -> if h = a then compress (a :: t) else h :: (compress t)
  | x -> x;;

(* Actual Solution *)
let rec compress_a = function
  | a :: (b :: _ as t) -> List.rev (if a = b then compress t else a :: compress t)
  | smaller -> smaller;;
