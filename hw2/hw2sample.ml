(* type mygram_nt = Mexpr | Mbinop | Mnum

let mygram_hw1 = (Mexpr,
  [Mexpr, [N Mnum; N Mbinop; N Mnum];
   Mexpr, [N Mnum];
   Mbinop, [T "+"];
   Mbinop, [T "-"];
   Mnum, [T "0"];
   Mnum, [T "1"]])

let mygram = (Mexpr,
  function
    | Mexpr ->
      [[N Mnum; N Mbinop; N Mnum];
       [N Mnum]]
    | Mbinop ->
      [[T "+"];
       [T "-"]]
    | Mnum ->
      [[T "0"];
       [T "1"]])

let mygram_cvt = convert_grammar mygram_hw1

let expr_rules = (snd mygram_cvt) Mexpr
let expr_rules = (snd mygram_cvt) Mbinop
let expr_rules = (snd mygram_cvt) Mnum;;

let fn1 = snd mygram and fn2 = snd mygram_cvt in
fn1 Mexpr = fn2 Mexpr && fn1 Mbinop = fn2 Mbinop && fn1 Mnum = fn2 Mnum *)

let test5 =
  (parse_tree_leaves (Node ("+", [Leaf 3; Node ("*", [Leaf 4; Leaf 5])]))
   = [3; 4; 5])

   let accept_all string = Some string
   let accept_empty_suffix = function
      | _::_ -> None
      | x -> Some x
   
   (* An example grammar for a small subset of Awk.
      This grammar is not the same as Homework 1; it is
      instead the grammar shown above.  *)
   
   type awksub_nonterminals =
     | Expr | Term | Lvalue | Incrop | Binop | Num
   
   let awkish_grammar =
     (Expr,
      function
        | Expr ->
            [[N Term; N Binop; N Expr];
             [N Term]]
        | Term ->
      [[N Num];
       [N Lvalue];
       [N Incrop; N Lvalue];
       [N Lvalue; N Incrop];
       [T"("; N Expr; T")"]]
        | Lvalue ->
      [[T"$"; N Expr]]
        | Incrop ->
      [[T"++"];
       [T"--"]]
        | Binop ->
      [[T"+"];
       [T"-"]]
        | Num ->
      [[T"0"]; [T"1"]; [T"2"]; [T"3"]; [T"4"];
       [T"5"]; [T"6"]; [T"7"]; [T"8"]; [T"9"]])
   
   let test0 =
     ((make_matcher awkish_grammar accept_all ["ouch"]) = None)
   
   let test1 =
     ((make_matcher awkish_grammar accept_all ["9"])
      = Some [])
   
   let test2 =
     ((make_matcher awkish_grammar accept_all ["9"; "+"; "$"; "1"; "+"])
      = Some ["+"])
   
   let test3 =
     ((make_matcher awkish_grammar accept_empty_suffix ["9"; "+"; "$"; "1"; "+"])
      = None)
   
   (* This one might take a bit longer.... *)
   let test4 =
    ((make_matcher awkish_grammar accept_all
        ["("; "$"; "8"; ")"; "-"; "$"; "++"; "$"; "--"; "$"; "9"; "+";
         "("; "$"; "++"; "$"; "2"; "+"; "("; "8"; ")"; "-"; "9"; ")";
         "-"; "("; "$"; "$"; "$"; "$"; "$"; "++"; "$"; "$"; "5"; "++";
         "++"; "--"; ")"; "-"; "++"; "$"; "$"; "("; "$"; "8"; "++"; ")";
         "++"; "+"; "0"])
     = Some [])