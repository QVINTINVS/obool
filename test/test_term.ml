open OUnit2
open Boolean_expression_simplifier
open Term

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

let test_common_factor_is_idempotent _ =
  (* ACD · B' *)
  let given_term = term4 0b1101 0b0010 in
  let actual_factor = common_factor given_term given_term in
  let expected_factor = given_term in
  assert_equal expected_factor actual_factor
;;

let test_common_factor_with_zero _ =
  (* ABCD *)
  let given_term_abcd = term4 0b1111 0b0000 in
  (* 0 *)
  let given_zero_term = term4 0b0000 0b0000 in
  let actual_factor = common_factor given_term_abcd given_zero_term in
  let expected_factor = term4 0b0000 0b0000 in
  assert_equal expected_factor actual_factor
;;

let common_factor_test_suite =
  "common_factor"
  >::: [ "identical_literals" >:: test_common_factor_of_identical_literals
       ; "disjoint_terms" >:: test_common_factor_of_disjoint_terms
       ; "preserves_shared_literals"
         >:: test_common_factor_preserves_shared_literals
       ; "commutativity" >:: test_common_factor_is_commutative
       ; "idempotency" >:: test_common_factor_is_idempotent
       ; "with_zero" >:: test_common_factor_with_zero
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
       ]
;;

let test_variables_returns_all_absent _ =
  (* 0 *)
  let given_term = term4 0b0000 0b0000 in
  let actual_variables = variables given_term |> List.of_seq in
  let expected_variables =
    [ Absent; Absent; Absent; Absent; Absent; Absent; Absent; Absent ]
  in
  assert_equal expected_variables actual_variables
;;

let test_variables_returns_positive_literals _ =
  (* AB *)
  let given_term = term4 0b0011 0b0000 in
  let actual_variables = variables given_term |> List.of_seq in
  let expected_variables =
    [ Present; Present; Absent; Absent; Absent; Absent; Absent; Absent ]
  in
  assert_equal expected_variables actual_variables
;;

let test_variables_returns_negative_literals _ =
  (* A'B' *)
  let given_term = term4 0b0000 0b0011 in
  let actual_variables = variables given_term |> List.of_seq in
  let expected_variables =
    [ Absent; Absent; Absent; Absent; Present; Present; Absent; Absent ]
  in
  assert_equal expected_variables actual_variables
;;

let test_variables_returns_mixed_literals _ =
  (* AC · B' *)
  let given_term = term4 0b0101 0b0010 in
  let actual_variables = variables given_term |> List.of_seq in
  let expected_variables =
    [ Present; Absent; Present; Absent; Absent; Present; Absent; Absent ]
  in
  assert_equal expected_variables actual_variables
;;

let test_variables_preserves_lsb_first_order _ =
  (* AD · B'C' *)
  let given_term = term4 0b1001 0b0110 in
  let actual_variables = variables given_term |> List.of_seq in
  let expected_variables =
    [ Present; Absent; Absent; Present; Absent; Present; Present; Absent ]
  in
  assert_equal expected_variables actual_variables
;;

let variables_test_suite =
  "variables"
  >::: [ "all_absent" >:: test_variables_returns_all_absent
       ; "positive_literals" >:: test_variables_returns_positive_literals
       ; "negative_literals" >:: test_variables_returns_negative_literals
       ; "mixed_literals" >:: test_variables_returns_mixed_literals
       ; "lsb_first_order" >:: test_variables_preserves_lsb_first_order
       ]
;;

let test_of_variables_list_returns_all_absent _ =
  (* 0 *)
  let given_variables =
    [ Absent; Absent; Absent; Absent; Absent; Absent; Absent; Absent ]
  in
  let actual_term = of_variables_list ~variable_count:4 given_variables in
  let expected_term = term4 0b0000 0b0000 in
  assert_equal expected_term actual_term
;;

let test_of_variables_list_returns_positive_literals _ =
  (* AB *)
  let given_variables =
    [ Present; Present; Absent; Absent; Absent; Absent; Absent; Absent ]
  in
  let actual_term = of_variables_list ~variable_count:4 given_variables in
  let expected_term = term4 0b0011 0b0000 in
  assert_equal expected_term actual_term
;;

let test_of_variables_list_returns_negative_literals _ =
  (* A'B' *)
  let given_variables =
    [ Absent; Absent; Absent; Absent; Present; Present; Absent; Absent ]
  in
  let actual_term = of_variables_list ~variable_count:4 given_variables in
  let expected_term = term4 0b0000 0b0011 in
  assert_equal expected_term actual_term
;;

let test_of_variables_list_returns_mixed_literals _ =
  (* AC · B' *)
  let given_variables =
    [ Present; Absent; Present; Absent; Absent; Present; Absent; Absent ]
  in
  let actual_term = of_variables_list ~variable_count:4 given_variables in
  let expected_term = term4 0b0101 0b0010 in
  assert_equal expected_term actual_term
;;

let test_of_variables_list_preserves_lsb_first_order _ =
  (* AD · B'C' *)
  let given_variables =
    [ Present; Absent; Absent; Present; Absent; Present; Present; Absent ]
  in
  let actual_term = of_variables_list ~variable_count:4 given_variables in
  let expected_term = term4 0b1001 0b0110 in
  assert_equal expected_term actual_term
;;

let of_variables_list_test_suite =
  "of_variables_list"
  >::: [ "all_absent" >:: test_of_variables_list_returns_all_absent
       ; "positive_literals"
         >:: test_of_variables_list_returns_positive_literals
       ; "negative_literals"
         >:: test_of_variables_list_returns_negative_literals
       ; "mixed_literals" >:: test_of_variables_list_returns_mixed_literals
       ; "lsb_first_order" >:: test_of_variables_list_preserves_lsb_first_order
       ]
;;

let suite =
  "term_operations_suite"
  >::: [ common_factor_test_suite
       ; normalization_test_suite
       ; variables_test_suite
       ; of_variables_list_test_suite
       ]
;;

let () = run_test_tt_main suite
