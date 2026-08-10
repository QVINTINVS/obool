open OUnit2
open Boolean_expression_simplifier
open Sum_of_products

let term = Term.of_lsb_ints

let test_insert_first_term_creates_root_path _ =
  (* A *)
  let trie = empty ~variable_count:2 |> add_term (term 0b01 0b00) in
  match trie.root with
  | Empty -> assert_failure "The trie should no longer be empty."
  | Node _ | Terminal -> ()
;;

let test_inserting_the_same_term_twice_keeps_the_same_structure _ =
  (* A *)
  let term = term 0b01 0b00 in
  let once = empty ~variable_count:2 |> add_term term in
  let twice = once |> add_term term in
  assert_equal once twice
;;

let test_inserting_terms_with_common_prefix_shares_nodes _ =
  let trie =
    empty ~variable_count:3
    (* A B *)
    |> add_term (term 0b011 0b000)
    (* A C *)
    |> add_term (term 0b101 0b000)
  in
  let absent, present =
    match trie.root with
    | Empty -> assert_failure "The trie should not be empty."
    | Terminal -> assert_failure "The root should not be terminal."
    | Node { absent; present } -> absent, present
  in
  match absent, present with
  | Empty, Node _ -> ()
  | _ -> assert_failure "The terms should share the first node."
;;

let test_inserting_terms_with_distinct_first_bit_creates_two_branches _ =
  let trie =
    empty ~variable_count:2
    |> add_term (term 0b01 0b00)
    |> add_term (term 0b10 0b00)
  in
  let absent, present =
    match trie.root with
    | Empty -> assert_failure "The trie should not be empty."
    | Terminal -> assert_failure "The root should not be terminal."
    | Node { absent; present } -> absent, present
  in
  match absent, present with
  | Node _, Terminal -> ()
  | _ -> assert_failure "The terms should create two distinct branches."
;;

let test_insert_single_literal_stops_at_terminal _ =
  let trie = empty ~variable_count:2 |> add_term (term 0b01 0b00) in
  match trie.root with
  | Empty -> assert_failure "The trie should not be empty."
  | Terminal -> assert_failure "The root should not be terminal."
  | Node { absent; present } ->
    assert_equal Empty absent;
    assert_equal Terminal present
;;

let test_insert_full_term_reaches_terminal_at_last_literal _ =
  let trie = empty ~variable_count:2 |> add_term (term 0b11 0b00) in
  let first_absent, first_present =
    match trie.root with
    | Empty -> assert_failure "The trie should not be empty."
    | Terminal -> assert_failure "The root should not be terminal."
    | Node { absent; present } -> absent, present
  in
  assert_equal Empty first_absent;
  let second_absent, second_present =
    match first_present with
    | Empty -> assert_failure "The first literal should be present."
    | Terminal -> assert_failure "The term should contain another literal."
    | Node { absent; present } -> absent, present
  in
  assert_equal Empty second_absent;
  assert_equal Terminal second_present
;;

let insertion_test_suite =
  "sum_of_products_insert"
  >::: [ "first_term" >:: test_insert_first_term_creates_root_path
       ; "duplicate_term"
         >:: test_inserting_the_same_term_twice_keeps_the_same_structure
       ; "common_prefix"
         >:: test_inserting_terms_with_common_prefix_shares_nodes
       ; "distinct_first_bit"
         >:: test_inserting_terms_with_distinct_first_bit_creates_two_branches
       ; "single_literal" >:: test_insert_single_literal_stops_at_terminal
       ; "full_term" >:: test_insert_full_term_reaches_terminal_at_last_literal
       ]
;;

let _ = run_test_tt_main insertion_test_suite
