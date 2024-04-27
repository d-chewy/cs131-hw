type ('nonterminal, 'terminal) symbol =
  | N of 'nonterminal
  | T of 'terminal

type ('nonterminal, 'terminal) parse_tree =
  | Node of 'nonterminal * ('nonterminal, 'terminal) parse_tree list
  | Leaf of 'terminal

(* Reverse list order *)
let rev_list list = 
  let rec rev_helper acc l = match l with
  | h::t -> rev_helper (h::acc) t
  | [] -> acc
in rev_helper [] list

(* Q1 *)
let convert_grammar (start, rules) = 
  let production_function nt = 
    let rec fun_helper acc r = match r with
    | (lhs,rhs) :: t -> if lhs = nt then fun_helper (rhs::acc) t else fun_helper acc t
    | [] -> acc
  in rev_list (fun_helper [] rules) (* to match input order for my OCD *)
in (start, production_function)

(* Q2 *)
let parse_tree_leaves tree = 
  let rec construct_list tr = match tr with
    | [] -> []
    | (Leaf x)::t -> x::(construct_list t)
    | (Node (_,y))::t -> (construct_list y)@(construct_list t)
  in match tree with (* Tree can start with either a Leaf or a Node. *)
    | Leaf x -> [x]
    | Node(_,y) -> construct_list y