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