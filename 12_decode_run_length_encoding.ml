type 'a rle =
  | One of 'a
  | Many of int * 'a;;

(* My Solution *)
let decode lst =
  let rec append n c lst =
    if n > 0 then append (pred n) c (c :: lst) else lst
  in
  let rec aux acc = function
  | [] -> acc
  | (Many (n, x)) :: t -> aux (append n x acc) t
  | (One n) :: t -> aux (n :: acc) t
  in List.rev @@ aux [] lst

let test_list_real = ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"e";"e";"e";"e"]
let test_list = [Many (4, "a"); One "b"; Many (2, "c"); Many (2, "a"); One "d";
Many (4, "e")]
let decoded_test_list = decode test_list
let result = test_list_real = decoded_test_list

(* Real Solution *)
let decode list =
  let rec many acc n x =
    if n = 0 then acc else many (x :: acc) (n-1) x in
  let rec aux acc = function
    | [] -> acc
    | One x :: t -> aux (x :: acc) t
    | Many (n,x) :: t -> aux (many acc n x) t  in
  aux [] (List.rev list)