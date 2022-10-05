(* My Solution *)
type 'a rle =
  | Many of 'a * int
  | One of 'a

let return c = function
  | 0 -> failwith "Impossible"
  | 1 -> One c
  | n -> Many (c, n)

let encode lst =
  let rec count_duplicates h n = function
    | [] -> (n, [])
    | h' :: t as l -> 
      if h' = h then count_duplicates h (succ n) t else (n, l)
  in
  let rec aux acc = function
  | [] -> acc
  | h :: t -> 
    let (count, rem) = count_duplicates h 1 t in
    aux ((return h count) :: acc) rem
  in List.rev @@ aux [] lst

(* Actual Solution *)
let encode list =
  let rle count x = if count = 0 then One x else Many (count + 1, x) in
  let rec aux count acc = function
    | [] -> [] (* Can only be reached if original list is empty *)
    | [x] -> rle count x :: acc
    | a :: (b :: _ as t) -> if a = b then aux (count + 1) acc t
                            else aux 0 (rle count a :: acc) t  in
  List.rev (aux 0 [] list);;