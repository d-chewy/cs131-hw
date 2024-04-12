(* let uniqList = let rec uniq a = function
  | [] -> a
  | h::t -> if inList h a then uniq a t else uniq (h::a) t
in uniq [] *)

(* let rec uniqList x = match x with
  | [] -> []
  | (h::t) -> if (inList [h] x) then []::(uniqList t) else h::(uniqList t) *)

(* Q1 
   ===============*)
let rec inList a b = match b with
  | [] -> false
  | (h::t) -> if a = h then true else inList a t;;

let rec subset a b = match a with
  | [] -> true
  | a_h :: a_t -> if (inList a_h b) then (subset a_t b) else false;;

(* Q2 
   ===============*)
let rec equal_sets a b = if subset a b && subset b a then true else false;;

(* Q3 
   ===============*)
(* let rec set_union a = function
  | [] -> a
  | h::t -> set_union (h::a) t;; *)

let set_union a b = a@b;;

(* Q4
   ============== *)
let rec set_all_union a = match a with
  | [] -> []
  | h::t -> set_union h (set_all_union t);;

(* Q5
   ============== 
  It is not possible to write such a function in OCaml because the language is
  generally designed to avoid self-referential data structures. Moreover, the 
  paradox still exists: how can a set be a member of itself if at times it is defined 
  as non-members? Being able to write such a function means it would be possible 
  to circularly reference a set from within itself through one of its members, as 
  one of its members would be the set itself.
*)

(* Q6
   ============== *)
let rec computed_fixed_point eq f x = let fix_pt = f x in if (eq fix_pt x) then x else computed_fixed_point (eq) (f) fix_pt;;
(* let computed_fixed_point eq f x = *)

(* Q7
   ============== *)
(* let rec computed_periodic_point eq f p x;; *)
let rec computed periodic point eq f p x = 