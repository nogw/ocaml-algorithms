let rec bubbleSort lst =
  let aux =
  match lst with
  | h :: h' :: tl when h > h' ->
    h' :: bubbleSort (h :: tl)
  | h :: tl ->
    h :: bubbleSort tl
  | tl ->
    tl
  in
    if lst = aux then lst
    else bubbleSort aux