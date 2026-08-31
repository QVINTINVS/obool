open OUnit2
open QCheck
open Boolean_expression_simplifier
open Term
module SOP = Sum_of_products
open Simplifier

let arb_term4 = make Gen.(map2 term4 (int_bound 15) (int_bound 15))

(* Generates non-empty normalized terms by sampling variable states directly *)
let arb_normalized_term4 =
  let valid_pairs =
    (fun positive -> List.init 16 (fun negated -> positive, negated))
    |> List.init 16
    |> List.concat_map
         (List.filter (fun (pos, neg) -> pos lor neg <> 0 && pos land neg = 0))
  in
  make Gen.(oneof_list valid_pairs |> map (fun (pos, neg) -> term4 pos neg))
;;

let arb_contradictory_term4 =
  make Gen.(map (fun mask -> term4 mask mask) (int_range 1 15))
;;

let qcheck_normalized_term_is_inserted =
  Test.make
    ~name:"add_simple_term inserts an already normalized term"
    arb_normalized_term4
    (fun term ->
       let simplifier = empty ~variable_count:4 |> add_simple_term term in
       match return simplifier with
       | One -> false
       | SOP actual ->
         let expected = SOP.empty ~variable_count:4 |> SOP.add_term term in
         actual = expected)
;;

let qcheck_normalized_term_is_idempotent =
  Test.make
    ~name:"add_simple_term is idempotent for normalized terms"
    arb_normalized_term4
    (fun term ->
       let once = empty ~variable_count:4 |> add_simple_term term in
       let twice = once |> add_simple_term term in
       return once = return twice)
;;

let qcheck_empty_term_is_ignored =
  Test.make ~name:"add_simple_term ignores the empty term" unit (fun _ ->
    let term = term4 0b0000 0b0000 in
    let initial = empty ~variable_count:4 in
    let result = initial |> add_simple_term term in
    return result = return initial)
;;

let qcheck_contradictory_term_is_ignored =
  Test.make
    ~name:"add_simple_term ignores contradictory terms"
    arb_contradictory_term4
    (fun term ->
       let initial = empty ~variable_count:4 in
       let result = initial |> add_simple_term term in
       return result = return initial)
;;

let qcheck_add_simple_term_follows_normalization =
  Test.make
    ~name:"add_simple_term follows Term.normalize"
    arb_term4
    (fun term ->
       let initial = empty ~variable_count:4 in
       let actual = initial |> add_simple_term term |> return in
       match normalize term with
       | Zero -> actual = return initial
       | Term normalized_term ->
         actual
         = SOP (SOP.empty ~variable_count:4 |> SOP.add_term normalized_term))
;;

let normalization_suite =
  "normalization"
  >::: QCheck_ounit.to_ounit2_test_list
         [ qcheck_normalized_term_is_inserted
         ; qcheck_normalized_term_is_idempotent
         ; qcheck_empty_term_is_ignored
         ; qcheck_contradictory_term_is_ignored
         ; qcheck_add_simple_term_follows_normalization
         ]
;;

let suite = "simplifier_operations_suite" >::: [ normalization_suite ]
let _ = run_test_tt_main suite
