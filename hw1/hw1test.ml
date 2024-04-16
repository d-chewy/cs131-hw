let my_subset_test0 = 
  subset ["three"; "body"; "problem"] ["three"; "body"; "fortnite"; "problem"];;
let my_subset_test1 = 
  subset ["three"; "body"; "problem"] ["three"; "fortnite"; "problem"];;

let my_equal_sets_test0 = 
  not (equal_sets ["three"; "body"; "problem"] ["three"; "body"; "fortnite"; "problem"]);;

let my_equal_sets_test1 = 
  equal_sets ["three"; "body"; "problem"] ["three"; "body"; "problem"];;

let my_set_union_test0 = 
  equal_sets (set_union ["problem"; "problem"] ["three"; "body"]) ["three"; "body"; "problem"];;

let my_set_union_test1 = 
  equal_sets (set_union [(1,2); (3,4); (5,6)] []) [(1,2); (3,4); (5,6)];;

let my_set_all_union_test0 = 
  equal_sets (set_all_union [["the"; "three"; "body"; "problem"]; ["alexander"; "the"; "great"]; ["dogma"]])
  ["the"; "three"; "body"; "problem";"alexander"; "the"; "great";"dogma"];;

let my_computed_fixed_point_test0 = 
  computed_fixed_point (=) (fun x -> x *. x *. x) 2. = infinity

let rec help_test_func0 x = match x with
  | [] -> []
  | h::t -> if h = "a" then "a"::(help_test_func0 t) else ""::(help_test_func0 t)
let my_computed_fixed_point_test0 = 
  computed_fixed_point (=) (help_test_func0) [] = []

let my_computed_fixed_point_test0 = 
  computed_fixed_point (=) (help_test_func0) ["a"; "banana"; "aa"; "a"] = ["a"; ""; ""; "a"]

let my_computed_periodic_test0 = 
  computed_periodic_point (=) (fun x -> x *. x /. 10.) 4 2. = 0.

let my_whilseq_test0 = 
  whileseq (( * ) 2) ((>) 256) 2 = [2; 4; 8; 16; 32; 64; 128]

type test_nonterminals = 
  | Strategy | Call | Site | Ability | Agent | Quiet 
let test_grammar = 
  Strategy,
  [
    Quiet, [];
    Strategy, [T"Start"; N Call];
    Call, [T"End"; N Site; N Ability; N Quiet];
    Call, [T"Use"; N Ability;];
    Call, [T"Cut"; N Quiet];
    Site, [T"A"]; Site, [T"B"];  Site, [T"C"];
    Ability, [T"Info"]; Ability, [T"Clear"]; Ability, [T"Hold"];
    Agent, [T"Jett"]; Agent, [T"Phoenix"]; Agent, [T"Sage"];
  ]
let my_filter_blind_alleys_test0 = filter_blind_alleys test_grammar = test_grammar;;

let my_filter_blind_alleys_test1 = filter_blind_alleys (Strategy, List.tl(snd test_grammar))
  = (Strategy,
  [(Strategy, [T "Start"; N Call]); (Call, [T "Use"; N Ability]);
   (Site, [T "A"]); (Site, [T "B"]); (Site, [T "C"]); (Ability, [T "Info"]);
   (Ability, [T "Clear"]); (Ability, [T "Hold"]); (Agent, [T "Jett"]);
   (Agent, [T "Phoenix"]); (Agent, [T "Sage"])])

let my_filter_blind_alleys_test2 = filter_blind_alleys (Call, List.tl(List.tl(List.tl(snd test_grammar))))
  = (Call,
  [(Call, [T "Use"; N Ability]); (Site, [T "A"]); (Site, [T "B"]);
   (Site, [T "C"]); (Ability, [T "Info"]); (Ability, [T "Clear"]);
   (Ability, [T "Hold"]); (Agent, [T "Jett"]); (Agent, [T "Phoenix"]);
   (Agent, [T "Sage"])])