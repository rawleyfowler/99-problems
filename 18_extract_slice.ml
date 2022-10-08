(* My Solution *)
let slice lst i k =
  let rec aux n = function
  | [] -> []
  | _ :: t as l -> if n = 0 then l else aux (pred n) t
  in List.rev (aux k (List.rev (aux i lst)))

(* Actual Solution *)
let slice list i k =
  let rec take n = function
    | [] -> []
    | h :: t -> if n = 0 then [] else h :: take (n-1) t
  in
  let rec drop n = function
    | [] -> []
    | h :: t as l -> if n = 0 then l else drop (n-1) t
  in
  take (k - i + 1) (drop i list);;