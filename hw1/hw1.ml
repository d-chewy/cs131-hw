(* let uniqList = let rec uniq a = function
  | [] -> a
  | h::t -> if inList h a then uniq a t else uniq (h::a) t
in uniq [] *)

(* let rec uniqList x = match x with
  | [] -> []
  | (h::t) -> if (inList [h] x) then []::(uniqList t) else h::(uniqList t) *)

type ('nonterminal, 'terminal) symbol =
  | N of 'nonterminal
  | T of 'terminal

(* Q1 
   ===============*)

   (* Helper function that determines whether an element is within the given list *)
let rec inList a b = match b with
  | [] -> false
  | (h::t) -> if a = h then true else inList a t;;

  (* Subset, generalization *)
let rec subset a b = match a with
  | [] -> true
  | a_h :: a_t -> if (inList a_h b) then (subset a_t b) else false;;

(* Q2 
   ===============*)

   (* If a is a subset of b and b is a subset of a, then they are equal sets. *)
let rec equal_sets a b = if subset a b && subset b a then true else false;;

(* Q3 
   ===============*)

   (* Using :: to construct union *)
(* let rec set_union a = function
  | [] -> a
  | h::t -> set_union (h::a) t;; *)

  (* Using @ to concatenate and construct union *)
let set_union a b = a@b;;

(* Q4
   ============== *)

   (* Union of sets *)
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

   (* Recursive function, find value for which f x = x (periodic point of p=1) *)
let rec computed_fixed_point eq f x = let fix_pt = f x 
  in if eq fix_pt x then x else computed_fixed_point eq f fix_pt;;

(* Q7
   ============== *)
(* let rec computed_periodic_point eq f p x;; *)
(* let rec computed_periodic_point eq f p x = 
  let rec per_x f p x = if p > 0 then per_x f (p-1) (f x) else x 
  in 
    let per_pt = per_x f p x in 
    if eq per_pt x then x else computed_periodic_point eq f p per_pt  *)
(* 
let rec computed_periodic_point eq f p x = 
  let rec iterate_i i p = if i < (p-1) then i + 1 else i
  in let rec compute_per_pt i = (
    let rec period_x f n x = 
      if n > 0 then period_x f (n-1) (f x) else x
    in let initial_x i = period_x f i x 
    in let next_x i = period_x f (i+p) x
    in if (iterate_i i p) = (p-1) 
      then computed_periodic_point eq f p (period_x f p x) 
      else
      if eq (next_x i) (initial_x i) 
        then x 
        else compute_per_pt (iterate_i i p)
  ) 
  in compute_per_pt 0 *)

(* let rec computed_periodic_point eq f (p : int) (x : float) = 
  let rec per_pt_func i px = 
    if i < p then per_pt_func (i+1) (f px) else px
  in let per_pt = per_pt_func 0 
in if eq (per_pt x) x then x else computed_periodic_point eq f p (f x);; *)

(* Recursive, computes f ( f ( f ( ... x ))) s.t. f is applied p times. 
   Checks if p applications generates x. Simply checks this for every f x *)
let rec computed_periodic_point eq f (p) (x) = 
  let rec per_pt_func i px = match i with
    | 0 -> px
    | _ -> per_pt_func (i-1) (f px)
    (* if i < p then per_pt_func (i+1) (f px) else px *)
  in let per_pt = per_pt_func p 
in if eq (per_pt x) x then x else computed_periodic_point eq f p (f x);;

(* Q8
   ================ *)

   (* Recursive list construction using ::*)
let rec whileseq s p x = 
  if p x then x::whileseq s p (s x) else []

(* Q9
    ================ *)
(* 
let rec acc_rules rules terminable_list = match rules with
  | (sym, rhs)::t -> 
    if rhs_terminable rhs terminable_list 
      then acc_rules t (sym::terminable_list)
      else acc_rules t terminable_list
  | _ -> terminable_list;;

let compute_rules rules terminable_list = 
  let sym_list_eq terminable_list0 terminable_list1 = 
    if equal_sets terminable_list0 terminable_list1 then true else false
  in
  computed_fixed_point (sym_list_eq) (acc_rules_f) (terminable_list);;

let filter_blind_alleys g = 
  let rec filter_rules (rules, terminable_list) = match rules with
    | h::t -> if rhs_terminable (snd h) terminable_list)
                then h::(filter_rules t terminable_list 
                else filter_rules t terminable_list
    | _ -> []
  in
  match g with
  (start, rules) -> (start, filter_rules (compute_rules rules []));; *)


(* Checks if the right hand side and all the symbols within the 
  right hand side terminate using a given list of symbols proven to terminate *)
let rec rhs_terminable rhs terminable_list = 
  let sym_terminable sym terminable_list = 
    match sym with 
    T s -> true
    | N s -> (inList sym terminable_list)
  in
	match rhs with 
	[] -> true
	| h::t when sym_terminable h terminable_list -> rhs_terminable t terminable_list
	| _ -> false;;

(* Accumulate list of symbols that terminate/construct productive rules list.
  Return a tuple to keep types consistent*)
let rec acc_rules_f (rules, terminable_list) = 
  let rec acc_rules rules terminable_list = match rules with
    | (s,rhs)::t -> if rhs_terminable rhs terminable_list
              then acc_rules t ((N s)::terminable_list)
              else acc_rules t terminable_list
    | _ -> terminable_list
  in (rules, acc_rules rules terminable_list)

(* Using computed_fixed_point, I can repeatedly call acc_rules_f which constantly
   generates a more correct list of productive rules as we "iterate" through 
   the provided grammar*)
let compute_rules rules terminable_list = 
  let term_list_eq (rules0, terminable_list0) (rules1,terminable_list1) =
    if equal_sets terminable_list0 terminable_list1
      then true
      else false
  in
	computed_fixed_point (term_list_eq) (acc_rules_f) (rules,terminable_list) 

(* Sub-function filter_rules checks if each rule provided in the grammar
   terminates using the above helper function rhs_terminable, and generates the 
   final output list which is the grammar without blind-alley rules.
   Pattern matched input to ignore the starting non-terminal symbol *)
let filter_blind_alleys g = 
  let rec filter_rules (rules, terminable_list) = 
    match rules with 
    h::t -> if rhs_terminable (snd h) terminable_list
              then h::filter_rules (t, terminable_list)
              else filter_rules (t, terminable_list)
    | _ -> []
  in
	match g with 
	(start, rules) -> (start, filter_rules (compute_rules rules []))