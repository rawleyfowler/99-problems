let reverse lst =
  let rec aux acc = function
  | [] -> acc
  | (h :: t) -> aux (h :: acc) t
  in aux [] lst;;

let is_palindrome lst = reverse lst |> ( = ) lst;;