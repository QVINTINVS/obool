open OUnit2
open Boolean_expression_simplifier
open Term

let term2 = of_lsb_ints ~variable_count:2
let term4 = of_lsb_ints ~variable_count:4

let test_common_factor_of_identical_literals _ =
  (* ABCD *)
  let given_term_abcd = term4 0b1111 0b0000 in
  (* AD · A'B'C'D' *)
  let given_term_ad_not_abcd = term4 0b1001 0b1111 in
  let actual_factor = common_factor given_term_abcd given_term_ad_not_abcd in
  (* AD *)
  let expected_factor_ad = term4 0b1001 0b0000 in
  assert_equal expected_factor_ad actual_factor
;;

let test_common_factor_of_disjoint_terms _ =
  (* AC · B'D' *)
  let given_term_ac_not_bd = term4 0b0101 0b1010 in
  (* BD · A'C' *)
  let given_term_bd_not_ac = term4 0b1010 0b0101 in
  let actual_factor = common_factor given_term_ac_not_bd given_term_bd_not_ac in
  (* 0 *)
  let expected_factor_empty = term4 0b0000 0b0000 in
  assert_equal expected_factor_empty actual_factor
;;

let test_common_factor_preserves_shared_literals _ =
  (* ACD · B' *)
  let given_term_acd_not_b = term4 0b1101 0b0010 in
  (* A · B' *)
  let given_term_a_not_b = term4 0b0001 0b0010 in
  let actual_factor = common_factor given_term_acd_not_b given_term_a_not_b in
  (* A · B' *)
  let expected_factor_a_not_b = term4 0b0001 0b0010 in
  assert_equal expected_factor_a_not_b actual_factor
;;

let test_common_factor_is_commutative _ =
  (* AC · B' *)
  let given_first_term = term4 0b0101 0b0010 in
  (* AD · C' *)
  let given_second_term = term4 0b1001 0b0100 in
  let actual_first_second = common_factor given_first_term given_second_term in
  let actual_second_first = common_factor given_second_term given_first_term in
  assert_equal actual_first_second actual_second_first
;;

let common_factor_test_suite =
  "common_factor"
  >::: [ "identical_literals" >:: test_common_factor_of_identical_literals
       ; "disjoint_terms" >:: test_common_factor_of_disjoint_terms
       ; "preserves_shared_literals"
         >:: test_common_factor_preserves_shared_literals
       ; "commutativity" >:: test_common_factor_is_commutative
       ]
;;

let test_normalize_rejects_empty_term _ =
  (* 0 *)
  let given_empty_term = term4 0b0000 0b0000 in
  let actual_result = normalize given_empty_term in
  let expected_result = Zero in
  assert_equal expected_result actual_result
;;

let test_normalize_rejects_contradictory_term _ =
  (* A · A' *)
  let given_term_a_not_a = term4 0b0001 0b0001 in
  let actual_result = normalize given_term_a_not_a in
  let expected_result = Zero in
  assert_equal expected_result actual_result
;;

let test_normalize_rejects_partially_contradictory_term _ =
  (* AB · A' *)
  let given_term_ab_not_a = term4 0b0011 0b0001 in
  let actual_result = normalize given_term_ab_not_a in
  let expected_result = Zero in
  assert_equal expected_result actual_result
;;

let test_normalize_rejects_multiple_contradictions _ =
  (* AC · A'C' *)
  let given_term_ac_not_ac = term4 0b0101 0b0101 in
  let actual_result = normalize given_term_ac_not_ac in
  let expected_result = Zero in
  assert_equal expected_result actual_result
;;

let test_normalize_preserves_consistent_term _ =
  (* AB · C' *)
  let given_valid_term_ab_not_c = term4 0b0011 0b0100 in
  let actual_result = normalize given_valid_term_ab_not_c in
  let expected_result = Term given_valid_term_ab_not_c in
  assert_equal expected_result actual_result
;;

let test_normalization_is_idempotent _ =
  let given_term_ab_not_c = term4 0b0011 0b0100 in
  let given_first_pass = normalize given_term_ab_not_c in
  let actual_result =
    match given_first_pass with
    | Term normalized_term -> normalize normalized_term
    | Zero -> Zero
  in
  let expected_result = given_first_pass in
  assert_equal expected_result actual_result
;;

let normalization_test_suite =
  "normalization"
  >::: [ "rejects_empty" >:: test_normalize_rejects_empty_term
       ; "rejects_contradictory" >:: test_normalize_rejects_contradictory_term
       ; "rejects_partially_contradictory"
         >:: test_normalize_rejects_partially_contradictory_term
       ; "rejects_multiple_contradictions"
         >:: test_normalize_rejects_multiple_contradictions
       ; "preserves_consistent" >:: test_normalize_preserves_consistent_term
       ; "idempotency" >:: test_normalization_is_idempotent
       ]
;;

let test_literal_occurrence_positive_variables _ =
  (* AC *)
  let given_term_ac = term4 0b0101 0b0000 in
  assert_equal Present (literal_occurrence given_term_ac 0);
  assert_equal Absent (literal_occurrence given_term_ac 1);
  assert_equal Present (literal_occurrence given_term_ac 2);
  assert_equal Absent (literal_occurrence given_term_ac 3)
;;

let test_literal_occurrence_negated_variables _ =
  (* B'D' *)
  let given_term_not_bd = term4 0b0000 0b1010 in
  assert_equal Absent (literal_occurrence given_term_not_bd 4);
  assert_equal Present (literal_occurrence given_term_not_bd 5);
  assert_equal Absent (literal_occurrence given_term_not_bd 6);
  assert_equal Present (literal_occurrence given_term_not_bd 7)
;;

let literal_occurrence_test_suite =
  "literal_occurrence"
  >::: [ "positive_variables" >:: test_literal_occurrence_positive_variables
       ; "negated_variables" >:: test_literal_occurrence_negated_variables
       ]
;;

let test_all_zero_from_before_literal _ =
  (* AB *)
  let given_term_ab = term2 0b11 0b00 in
  assert_equal false (all_zero_from given_term_ab 0);
  assert_equal false (all_zero_from given_term_ab 1);
  assert_equal true (all_zero_from given_term_ab 2)
;;

let test_all_zero_from_negated_literal _ =
  (* AB' *)
  let given_term_a_not_b = term2 0b01 0b10 in
  assert_equal false (all_zero_from given_term_a_not_b 2);
  assert_equal false (all_zero_from given_term_a_not_b 3);
  assert_equal true (all_zero_from given_term_a_not_b 4)
;;

let test_all_zero_from_zero_term _ =
  (* 0 *)
  let given_empty_term = term2 0b00 0b00 in
  assert_equal true (all_zero_from given_empty_term 0);
  assert_equal true (all_zero_from given_empty_term 2);
  assert_equal true (all_zero_from given_empty_term 4)
;;

let test_all_zero_from_single_literal _ =
  (* A *)
  let given_term_a = term2 0b01 0b00 in
  assert_equal false (all_zero_from given_term_a 0);
  assert_equal true (all_zero_from given_term_a 1);
  assert_equal true (all_zero_from given_term_a 2);
  assert_equal true (all_zero_from given_term_a 4)
;;

let all_zero_from_test_suite =
  "all_zero_from"
  >::: [ "before_literal" >:: test_all_zero_from_before_literal
       ; "negated_literal" >:: test_all_zero_from_negated_literal
       ; "zero_term" >:: test_all_zero_from_zero_term
       ; "single_literal" >:: test_all_zero_from_single_literal
       ]
;;

let suite =
  "term_operations_suite"
  >::: [ common_factor_test_suite
       ; normalization_test_suite
       ; literal_occurrence_test_suite
       ; all_zero_from_test_suite
       ]
;;

let () = run_test_tt_main suite
