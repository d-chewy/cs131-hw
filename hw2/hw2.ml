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
let rec parse_tree_leaves tree = match tree with
  | Leaf t_sym -> [t_sym]
  | Node (_, child_h::child_t) -> (parse_tree_leaves child_h) @ (parse_node_children child_t)
  | _ -> []
and parse_node_children = function
  | [] -> []
  | (h::t) -> (parse_tree_leaves h) @ (parse_node_children t)

(* Q3: acceptor is a function, returns None or Some x for some value x. fragment is a list of terminals

  matcher is passed accept and frag. must match prefix p of frag s.t. accept (suffix of frag after p is removed). 
  if match -> return accept's return, else None
  try grammar rules in order, return result of accept on suffix. may not be shortest/longest
  
  what is acceptable? if accept succeeds on suffix fragment immediately following matching prefix

  iterate through grammar in order and see if element in the frag matches

*)
let make_matcher (start, prod_fun) acceptor fragment =

  (* Traverse nonterminal's expansion to determine full potential prefix *)
let rec match_syms sym_rules accept frag = match sym_rules with 
        | [] -> None
        | h::t -> (match match_prefix h accept frag with (* check first rule of alt list *)
                      | None -> match_syms t accept frag
                      | x -> x (* Accept suffix *)
        )
and match_prefix syms accept frag = match syms with
    | [] -> accept frag
    | _ -> (
      (* Find symbol that matches fragment head or symbol that COULD match frag head *)
      match frag with 
        | [] -> None
        | frag_h::frag_t -> (match syms with
            | [] -> None
            | h::t -> (match h with
              (* Look for next potential element of prefix *)
              | T t_sym -> if t_sym = frag_h then match_prefix t accept frag_t else None
              (* Expand nonterminal *)
              | N nt_sym -> match_syms (prod_fun nt_sym) (match_prefix t accept) frag
            )
        ))
in let helper =
    match_prefix [N start] acceptor fragment
in helper

let make_parser gram = None