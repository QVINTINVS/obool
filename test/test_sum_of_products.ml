open OUnit2
open Boolean_expression_simplifier
open Sum_of_products

let term = Term.of_lsb_ints

let test_dont_care_variables_are_omitted_from_representation _ =
  (* A *)
  let given_term_a = term 0b01 0b00 in
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
  let given_term_a = term 0b01 0b00 in
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
  let given_term_ab = term 0b011 0b000 in
  (* A C *)
  let given_term_ac = term 0b101 0b000 in
  let given_expression = empty ~variable_count:3 in
  let actual_expression =
    given_expression |> add_term given_term_ab |> add_term given_term_ac
  in
  (* AB + AC = A(B + C) *)
  let expected_expression =
    { variable_count = 3
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
  let given_term_a = term 0b01 0b00 in
  (* B *)
  let given_term_b = term 0b10 0b00 in
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
  let given_term_b = term 0b0010 0b0000 in
  (* B C *)
  let given_term_bc = term 0b0110 0b0000 in
  (* B D *)
  let given_term_bd = term 0b1010 0b0000 in
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
  let given_overextended_term_ab = term 0b1101 0b0010 in
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
                { absent = Node { absent = Empty; present = Terminal }
                ; present = Empty
                }
          }
    }
  in
  assert_equal expected_expression actual_expression
;;

let suite =
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

let _ = run_test_tt_main suite
