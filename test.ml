
let rec subset a b = 
  match a with 
  | [] -> true
  | h :: a -> subset a b