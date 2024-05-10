(* Q5 make_matcher test case *)
type my_nonterminals =
  | Sentence | Noun | Verb | Adjective | Article | Preposition | PluralNoun

let my_grammar =
  (Sentence,
   function
     | Sentence ->
         [[N Noun; N Verb];
          [N Article; N Adjective; N Noun; N Verb; N Preposition; N Noun];
          [N Article; N Adjective; N Noun; N Verb; N Preposition; N Article; N Adjective; N Noun]]
     | Noun ->
       [[T "cat"]; [T "dog"]; [T "bird"]; [N PluralNoun]]
     | PluralNoun ->
       [[T "cats"]; [T "dogs"]; [T "birds"];]
     | Verb ->
       [[T "jumps"]; [T "runs"]; [T "flies"]]
     | Adjective ->
       [[T "small"]; [T "big"]; [T "fast"]; [T "slow"]]
     | Article ->
       [[T "the"]; [T "a"]; [T "an"]]
     | Preposition ->
       [[T "over"]; [T "under"]; [T "around"]])

(* let my_acceptor string = Some string *)
let my_acceptor_2 string = Some ["under"; "the"; "small"; "cat"]

let make_matcher_test =
  (* assert (make_matcher my_grammar my_acceptor ["cat"; "jumps"] = Some [] );
  assert (make_matcher my_grammar my_acceptor ["cat"; "jumps"; "test"] = Some ["test"] );
  assert (make_matcher my_grammar my_acceptor ["the"; "big"; "dog"; "runs"; "around"; "birds"; "small"; "cat"] = Some ["small"; "cat"]);
  assert (make_matcher my_grammar my_acceptor ["a"; "fast"; "bird"; "flies"; "over"; "a"; "slow"; "dog"] = Some []);
  assert (make_matcher my_grammar my_acceptor ["cat"; "fly"] = None);  (* Invalid input: Unknown verb *)
  assert (make_matcher my_grammar my_acceptor ["tiger"; "jumps"] = None);  (* Invalid input: Unknown noun *)
  assert (make_matcher my_grammar my_acceptor ["the"; "big"; "dog"; "under"; "the"; "small"; "cat"] = None);  Invalid input: Missing verb *)
  (make_matcher my_grammar my_acceptor_2 ["the"; "big"; "dog"; "flies"; "under"; "the"; "small"; "cats"; "swiftly"] = Some ["under"; "the"; "small"; "cat"]);  (* Invalid input: Missing verb *)
  (* assert (make_matcher my_grammar my_acceptor_3 ["the"; "big"; "dog"; "under"; "the"; "small"; "cat"] = Some ["under"; "the"; "small"; "cat"]);  Invalid input: Missing verb *)

(* make_matcher_test () *)

(* Q6 make_parser test case *)
(* Did not complete make_parser *)