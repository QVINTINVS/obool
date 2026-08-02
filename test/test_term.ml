open OUnit2
open Boolean_expression_simplifier
open Term

let bits = Bitv.of_int_us

let test_common_factor_of_identical_literals _ =
  (* A B C D *)
  let left = { positive_bits = bits 0b1111; negated_bits = bits 0b0000 } in
  (* A D · A'B'C'D' *)
  let right = { positive_bits = bits 0b1001; negated_bits = bits 0b1111 } in
  let expected = { positive_bits = bits 0b1001; negated_bits = bits 0b0000 } in
  assert_equal expected (common_factor left right)
;;

let test_common_factor_of_disjoint_terms _ =
  (* A C · B'D' *)
  let left = { positive_bits = bits 0b1010; negated_bits = bits 0b0101 } in
  (* B D · A'C' *)
  let right = { positive_bits = bits 0b0101; negated_bits = bits 0b1010 } in
  let expected = { positive_bits = bits 0b0000; negated_bits = bits 0b0000 } in
  assert_equal expected (common_factor left right)
;;

let test_common_factor_preserves_shared_literals _ =
  (* A C D · B' *)
  let left = { positive_bits = bits 0b1011; negated_bits = bits 0b0100 } in
  (* A · B' *)
  let right = { positive_bits = bits 0b1000; negated_bits = bits 0b0100 } in
  let expected = { positive_bits = bits 0b1000; negated_bits = bits 0b0100 } in
  assert_equal expected (common_factor left right)
;;

let test_common_factor_is_commutative _ =
  (* A C · B' *)
  let left = { positive_bits = bits 0b1010; negated_bits = bits 0b0100 } in
  (* A D · C' *)
  let right = { positive_bits = bits 0b1001; negated_bits = bits 0b0010 } in
  assert_equal (common_factor left right) (common_factor right left)
;;

let test_detects_contradiction _ =
  (* A appears both positively and negated *)
  let term = { positive_bits = bits 0b1000; negated_bits = bits 0b1000 } in
  assert_bool "Term should be contradictory" (is_contradictory term)
;;

let test_accepts_consistent_term _ =
  let term = { positive_bits = bits 0b1100; negated_bits = bits 0b0010 } in
  assert_bool "Term should not be contradictory" (not (is_contradictory term))
;;

let test_normalize_returns_zero_for_contradiction _ =
  let term = { positive_bits = bits 0b1000; negated_bits = bits 0b1000 } in
  assert_equal Zero (normalize term)
;;

let test_normalize_preserves_consistent_term _ =
  let term = { positive_bits = bits 0b1100; negated_bits = bits 0b0010 } in
  assert_equal (Term term) (normalize term)
;;

let test_normalization_is_idempotent _ =
  let term = { positive_bits = bits 0b1100; negated_bits = bits 0b0010 } in
  match normalize term with
  | Zero -> assert_failure "Expected a valid normalized term."
  | Term normalized_term ->
    assert_equal (Term normalized_term) (normalize normalized_term)
;;

let test_normalize_detects_multiple_contradictory_literals _ =
  (* A C and A' C' *)
  let term = { positive_bits = bits 0b1010; negated_bits = bits 0b1010 } in
  assert_equal Zero (normalize term)
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
       ; "detects_contradiction" >:: test_detects_contradiction
       ; "accepts_consistent_term" >:: test_accepts_consistent_term
       ; "normalize_returns_zero_for_contradiction"
         >:: test_normalize_returns_zero_for_contradiction
       ; "normalize_preserves_consistent_term"
         >:: test_normalize_preserves_consistent_term
       ; "normalization_is_idempotent" >:: test_normalization_is_idempotent
       ; "normalize_detects_multiple_contradictory_literals"
         >:: test_normalize_detects_multiple_contradictory_literals
       ]
;;

let () = run_test_tt_main suite
