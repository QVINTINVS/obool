open OUnit2
open Boolean_expression_simplifier
open Term
open Sum_of_products

let test_dont_care_variables_are_omitted_from_representation _ =
  (* A *)
  let given_term_a = term2 0b01 0b00 in
  let given_expression = empty ~variable_count:2 in
  let actual_expression = add_term given_term_a given_expression in
  (* A (B is don't-care) *)
  let expected_expression =
    { variable_count = 2; root = Node { absent = Empty; present = Terminal } }
  in
  assert_equal expected_expression actual_expression
;;

let test_addition_of_duplicate_term_is_idempotent _ =
  (* A *)
  let given_term_a = term2 0b01 0b00 in
  let given_expression_with_a =
    empty ~variable_count:2 |> add_term given_term_a
  in
  let actual_expression = add_term given_term_a given_expression_with_a in
  (* A + A = A *)
  let expected_expression = given_expression_with_a in
  assert_equal expected_expression actual_expression
;;

let test_terms_sharing_common_literal_factor_their_prefix _ =
  (* A B *)
  let given_term_ab = term4 0b0011 0b0000 in
  (* A C *)
  let given_term_ac = term4 0b0101 0b0000 in
  let given_expression = empty ~variable_count:4 in
  let actual_expression =
    given_expression |> add_term given_term_ab |> add_term given_term_ac
  in
  (* AB + AC = A(B + C) *)
  let expected_expression =
    { variable_count = 4
    ; root =
        Node
          { absent = Empty
          ; present =
              Node
                { absent = Node { absent = Empty; present = Terminal }
                ; present = Terminal
                }
          }
    }
  in
  assert_equal expected_expression actual_expression
;;

let test_disjoint_literals_form_independent_product_terms _ =
  (* A *)
  let given_term_a = term2 0b01 0b00 in
  (* B *)
  let given_term_b = term2 0b10 0b00 in
  let given_expression = empty ~variable_count:2 in
  let actual_expression =
    given_expression |> add_term given_term_a |> add_term given_term_b
  in
  (* A + B *)
  let expected_expression =
    { variable_count = 2
    ; root =
        Node
          { absent = Node { absent = Empty; present = Terminal }
          ; present = Terminal
          }
    }
  in
  assert_equal expected_expression actual_expression
;;

let test_preserves_non_irredundant_terms _ =
  (* B *)
  let given_term_b = term4 0b0010 0b0000 in
  (* B C *)
  let given_term_bc = term4 0b0110 0b0000 in
  (* B D *)
  let given_term_bd = term4 0b1010 0b0000 in
  let given_expression = empty ~variable_count:4 in
  let actual_expression =
    given_expression
    |> add_term given_term_b
    |> add_term given_term_bc
    |> add_term given_term_bd
  in
  (* B + BC + BD *)
  let expected_expression =
    { variable_count = 4
    ; root =
        Node
          { absent =
              Node
                { absent = Empty
                ; present =
                    Node
                      { absent = Node { absent = Terminal; present = Terminal }
                      ; present = Terminal
                      }
                }
          ; present = Empty
          }
    }
  in
  assert_equal expected_expression actual_expression
;;

let test_extra_bits_are_truncated _ =
  (* ACD · B' *)
  let given_overextended_term_ab = term4 0b1101 0b0010 in
  let given_expression = empty ~variable_count:2 in
  let actual_expression =
    add_term given_overextended_term_ab given_expression
  in
  (* ACD · B' -> A · B' (truncated to 2-variable universe) *)
  let expected_expression =
    { variable_count = 2
    ; root =
        Node
          { absent = Empty
          ; present =
              Node
                { present = Empty
                ; absent =
                    Node
                      { present = Empty
                      ; absent = Node { absent = Empty; present = Terminal }
                      }
                }
          }
    }
  in
  assert_equal expected_expression actual_expression
;;

let add_term_test_suite =
  "add_term"
  >::: [ "dont_care_omission"
         >:: test_dont_care_variables_are_omitted_from_representation
       ; "idempotency" >:: test_addition_of_duplicate_term_is_idempotent
       ; "common_prefix_factoring"
         >:: test_terms_sharing_common_literal_factor_their_prefix
       ; "independent_terms"
         >:: test_disjoint_literals_form_independent_product_terms
       ; "non_irredundant_terms" >:: test_preserves_non_irredundant_terms
       ; "extra_bits_truncated" >:: test_extra_bits_are_truncated
       ]
;;

let test_absorption_finds_an_existing_absorbing_term _ =
  (* A absorbs AB *)
  let given_term_a = term4 0b0001 0b0000 in
  let given_term_ab = term4 0b0011 0b0000 in
  let given_expression = empty ~variable_count:4 |> add_term given_term_a in
  let actual_term =
    get_first_term_that_absorves given_term_ab given_expression
  in
  assert_equal (Some given_term_a) actual_term
;;

let test_absorption_returns_none_for_an_empty_trie _ =
  (* No terms stored *)
  let given_term_ab = term4 0b0011 0b0000 in
  let given_expression = empty ~variable_count:4 in
  let actual_term =
    get_first_term_that_absorves given_term_ab given_expression
  in
  assert_equal None actual_term
;;

let test_absorption_distinguishes_positive_and_negative_literals _ =
  (* A does not absorb A' *)
  let given_term_a = term4 0b0001 0b0000 in
  let given_term_not_a = term4 0b0000 0b0001 in
  let given_expression = empty ~variable_count:4 |> add_term given_term_a in
  let actual_term =
    get_first_term_that_absorves given_term_not_a given_expression
  in
  assert_equal None actual_term
;;

let test_absorption_recognizes_negative_literals _ =
  (* A' absorbs A'B *)
  let given_term_not_a = term4 0b0000 0b0001 in
  let given_term_not_a_b = term4 0b0010 0b0001 in
  let given_expression = empty ~variable_count:4 |> add_term given_term_not_a in
  let actual_term =
    get_first_term_that_absorves given_term_not_a_b given_expression
  in
  assert_equal (Some given_term_not_a) actual_term
;;

let test_absorption_recognizes_mixed_literals _ =
  (* AB' absorbs AB'C *)
  let given_term_ab_not = term4 0b0001 0b0010 in
  let given_term_ab_not_c = term4 0b0101 0b0010 in
  let given_expression =
    empty ~variable_count:4 |> add_term given_term_ab_not
  in
  let actual_term =
    get_first_term_that_absorves given_term_ab_not_c given_expression
  in
  assert_equal (Some given_term_ab_not) actual_term
;;

let test_absorption_finds_a_match_among_multiple_terms _ =
  (* AB and B' do not absorb CD; C does. *)
  let given_term_ab = term4 0b0011 0b0000 in
  let given_term_not_b = term4 0b0000 0b0010 in
  let given_term_c = term4 0b0100 0b0000 in
  let given_term_cd = term4 0b1100 0b0000 in
  let given_expression =
    empty ~variable_count:4
    |> add_term given_term_ab
    |> add_term given_term_not_b
    |> add_term given_term_c
  in
  let actual_term =
    get_first_term_that_absorves given_term_cd given_expression
  in
  assert_equal (Some given_term_c) actual_term
;;

let test_absorption_returns_none_when_no_term_matches _ =
  (* A, B' and C do not absorb B. *)
  let given_term_a = term4 0b0001 0b0000 in
  let given_term_not_b = term4 0b0000 0b0010 in
  let given_term_c = term4 0b0100 0b0000 in
  let given_term_b = term4 0b0010 0b0000 in
  let given_expression =
    empty ~variable_count:4
    |> add_term given_term_a
    |> add_term given_term_not_b
    |> add_term given_term_c
  in
  let actual_term =
    get_first_term_that_absorves given_term_b given_expression
  in
  assert_equal None actual_term
;;

let test_absorption_finds_an_identical_term _ =
  (* AB absorbs AB *)
  let given_term_ab = term4 0b0011 0b0000 in
  let given_expression = empty ~variable_count:4 |> add_term given_term_ab in
  let actual_term =
    get_first_term_that_absorves given_term_ab given_expression
  in
  assert_equal (Some given_term_ab) actual_term
;;

let test_absorption_does_not_match_a_more_restrictive_term _ =
  (* AB does not absorb A *)
  let given_term_ab = term4 0b0011 0b0000 in
  let given_term_a = term4 0b0001 0b0000 in
  let given_expression = empty ~variable_count:4 |> add_term given_term_ab in
  let actual_term =
    get_first_term_that_absorves given_term_a given_expression
  in
  assert_equal None actual_term
;;

let test_absorption_returns_none_for_an_all_absent_candidate _ =
  let given_term_zero = term4 0b0000 0b0000 in
  let given_term_a = term4 0b0001 0b0000 in
  let given_expression = empty ~variable_count:4 |> add_term given_term_a in
  let actual_term =
    get_first_term_that_absorves given_term_zero given_expression
  in
  assert_equal None actual_term
;;

let absorption_test_suite =
  "absorption"
  >::: [ "existing_absorbing_term"
         >:: test_absorption_finds_an_existing_absorbing_term
       ; "empty_trie" >:: test_absorption_returns_none_for_an_empty_trie
       ; "literal_polarity"
         >:: test_absorption_distinguishes_positive_and_negative_literals
       ; "negative_literals" >:: test_absorption_recognizes_negative_literals
       ; "mixed_literals" >:: test_absorption_recognizes_mixed_literals
       ; "multiple_terms" >:: test_absorption_finds_a_match_among_multiple_terms
       ; "no_matching_term"
         >:: test_absorption_returns_none_when_no_term_matches
       ; "identical_term" >:: test_absorption_finds_an_identical_term
       ; "more_restrictive_term"
         >:: test_absorption_does_not_match_a_more_restrictive_term
       ; "all_absent_candidate"
         >:: test_absorption_returns_none_for_an_all_absent_candidate
       ]
;;

let suite =
  "sum_of_products_suite" >::: [ add_term_test_suite; absorption_test_suite ]
;;

let () = run_test_tt_main suite
