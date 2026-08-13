open OUnit2
open Boolean_expression_simplifier
open Term

let test_common_factor_of_identical_literals _ =
  (* A B C D *)
  let left = of_lsb_ints 0b1111 0b0000 in
  (* A D · A'B'C'D' *)
  let right = of_lsb_ints 0b1001 0b1111 in
  let expected = of_lsb_ints 0b1001 0b0000 in
  assert_equal expected (common_factor left right)
;;

let test_common_factor_of_disjoint_terms _ =
  (* A C · B'D' *)
  let left = of_lsb_ints 0b0101 0b1010 in
  (* B D · A'C' *)
  let right = of_lsb_ints 0b1010 0b0101 in
  let expected = of_lsb_ints 0b0000 0b0000 in
  assert_equal expected (common_factor left right)
;;

let test_common_factor_preserves_shared_literals _ =
  (* A C D · B' *)
  let left = of_lsb_ints 0b1101 0b0010 in
  (* A · B' *)
  let right = of_lsb_ints 0b0001 0b0010 in
  let expected = of_lsb_ints 0b0001 0b0010 in
  assert_equal expected (common_factor left right)
;;

let test_common_factor_is_commutative _ =
  (* A C · B' *)
  let left = of_lsb_ints 0b0101 0b0010 in
  (* A D · C' *)
  let right = of_lsb_ints 0b1001 0b0100 in
  assert_equal (common_factor left right) (common_factor right left)
;;

let test_normalize_rejects_empty_term _ =
  let term = of_lsb_ints 0b0000 0b0000 in
  assert_equal Zero (normalize term)
;;

let test_normalize_rejects_contradictory_term _ =
  (* A appears both positively and negated *)
  let term = of_lsb_ints 0b0001 0b0001 in
  assert_equal Zero (normalize term)
;;

let test_normalize_rejects_partially_contradictory_term _ =
  let term = of_lsb_ints 0b0011 0b0001 in
  assert_equal Zero (normalize term)
;;

let test_normalize_rejects_multiple_contradictions _ =
  (* A C and A' C' *)
  let term = of_lsb_ints 0b0101 0b0101 in
  assert_equal Zero (normalize term)
;;

let test_normalize_preserves_consistent_term _ =
  (* A B · C' *)
  let term = of_lsb_ints 0b0011 0b0100 in
  assert_equal (Term term) (normalize term)
;;

let test_normalization_is_idempotent _ =
  let term = of_lsb_ints 0b0011 0b0100 in
  match normalize term with
  | Zero -> assert_failure "Expected a valid normalized term."
  | Term normalized_term ->
    assert_equal (Term normalized_term) (normalize normalized_term)
;;

let test_literal_occurrence_positive_variables _ =
  (* A C *)
  let term = of_lsb_ints ~variable_count:4 0b0101 0b0000 in
  assert_equal Present (literal_occurrence term 0);
  assert_equal Absent (literal_occurrence term 1);
  assert_equal Present (literal_occurrence term 2);
  assert_equal Absent (literal_occurrence term 3)
;;

let test_literal_occurrence_negated_variables _ =
  (* B' D' *)
  let term = of_lsb_ints ~variable_count:4 0b0000 0b1010 in
  assert_equal Absent (literal_occurrence term 4);
  assert_equal Present (literal_occurrence term 5);
  assert_equal Absent (literal_occurrence term 6);
  assert_equal Present (literal_occurrence term 7)
;;

let test_all_zero_from_before_literal _ =
  (* A B *)
  let term = of_lsb_ints ~variable_count:2 0b11 0b00 in
  assert_equal false (all_zero_from term 0);
  assert_equal false (all_zero_from term 1);
  assert_equal true (all_zero_from term 2)
;;

let test_all_zero_from_negated_literal _ =
  (* A B' *)
  let term = of_lsb_ints ~variable_count:2 0b01 0b10 in
  assert_equal false (all_zero_from term 2);
  assert_equal false (all_zero_from term 3);
  assert_equal true (all_zero_from term 4)
;;

let test_all_zero_from_zero_term _ =
  let term = of_lsb_ints ~variable_count:2 0b00 0b00 in
  assert_equal true (all_zero_from term 0);
  assert_equal true (all_zero_from term 2);
  assert_equal true (all_zero_from term 4)
;;

let test_all_zero_from_single_literal _ =
  (* A *)
  let term = of_lsb_ints ~variable_count:2 0b01 0b00 in
  assert_equal false (all_zero_from term 0);
  assert_equal true (all_zero_from term 1);
  assert_equal true (all_zero_from term 2);
  assert_equal true (all_zero_from term 4)
;;

let suite =
  "term_operations"
  >::: [ "common_factor_of_identical_literals"
         >:: test_common_factor_of_identical_literals
       ; "common_factor_of_disjoint_terms"
         >:: test_common_factor_of_disjoint_terms
       ; "common_factor_preserves_shared_literals"
         >:: test_common_factor_preserves_shared_literals
       ; "common_factor_is_commutative" >:: test_common_factor_is_commutative
       ; "normalize_rejects_empty_term" >:: test_normalize_rejects_empty_term
       ; "normalize_rejects_contradictory_term"
         >:: test_normalize_rejects_contradictory_term
       ; "normalize_rejects_partially_contradictory_term"
         >:: test_normalize_rejects_partially_contradictory_term
       ; "normalize_rejects_multiple_contradictions"
         >:: test_normalize_rejects_multiple_contradictions
       ; "normalize_preserves_consistent_term"
         >:: test_normalize_preserves_consistent_term
       ; "normalization_is_idempotent" >:: test_normalization_is_idempotent
       ; "literal_occurrence_positive_variables"
         >:: test_literal_occurrence_positive_variables
       ; "literal_occurrence_negated_variables"
         >:: test_literal_occurrence_negated_variables
       ; "all_zero_from_before_literal" >:: test_all_zero_from_before_literal
       ; "all_zero_from_negated_literal" >:: test_all_zero_from_negated_literal
       ; "all_zero_from_zero_term" >:: test_all_zero_from_zero_term
       ; "all_zero_from_single_literal" >:: test_all_zero_from_single_literal
       ]
;;

let () = run_test_tt_main suite
