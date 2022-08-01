let rec last lst =
  match lst with
  | [] -> None
  | _ -> lst.[(List.length lst) - 1];;
