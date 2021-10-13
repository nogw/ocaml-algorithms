let split list =
  let rec aux l acc acc' =
    match l with
    | [] -> (acc, acc')
    | [x] -> (x :: acc, acc')
    | h :: h' :: t -> aux t (h :: acc) (h' :: acc')
  in
    aux list [] []

let rec mergeSort l l' =
  match (l, l') with
  | (x, []) -> x
  | ([], x) -> x
  | (x :: tx, y :: ty) -> 
    if x < y then
      x :: mergeSort tx l'
    else
      y :: mergeSort l ty

let rec sort list =
  match list with
  | [] -> []
  | [x] -> [x]
  | _ ->
    let (l, l') = split list in
    mergeSort (sort l) (sort l')