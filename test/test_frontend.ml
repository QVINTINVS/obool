open OUnit2
open Boolean_expression_simplifier
open Frontend

let assert_parses expression expected =
  let parsed = parse expression in
  assert_bool ("Unexpected AST for \"" ^ expression ^ "\"") (expected = parsed)
;;

let test_single_operand _ = assert_parses "A" (Operand "A")
let test_parenthesized_variable _ = assert_parses "(A)" (Operand "A")
let test_prefix_not _ = assert_parses "'A" (Not (Operand "A"))
let test_binary_and _ = assert_parses "A and B" (And (Operand "A", Operand "B"))
let test_binary_or _ = assert_parses "A or B" (Or (Operand "A", Operand "B"))
let test_binary_xor _ = assert_parses "A xor B" (Xor (Operand "A", Operand "B"))

let test_binary_nand _ =
  assert_parses "A nand B" (Nand (Operand "A", Operand "B"))
;;

let test_binary_nor _ = assert_parses "A nor B" (Nor (Operand "A", Operand "B"))

let test_binary_nxor _ =
  assert_parses "A nxor B" (Nxor (Operand "A", Operand "B"))
;;

let test_not_has_higher_precedence _ =
  assert_parses "'A and B" (And (Not (Operand "A"), Operand "B"))
;;

let test_and_has_higher_precedence_than_or _ =
  assert_parses
    "A and B or C"
    (Or (And (Operand "A", Operand "B"), Operand "C"))
;;

let test_left_associativity _ =
  assert_parses
    "A and B and C"
    (And (And (Operand "A", Operand "B"), Operand "C"))
;;

let test_nested_parentheses _ =
  assert_parses
    "(A and (B or C))"
    (And (Operand "A", Or (Operand "B", Operand "C")))
;;

let test_parser =
  "parser"
  >::: [ "single_variable" >:: test_single_operand
       ; "parenthesized_variable" >:: test_parenthesized_variable
       ; "prefix_not" >:: test_prefix_not
       ; "binary_and" >:: test_binary_and
       ; "binary_or" >:: test_binary_or
       ; "binary_xor" >:: test_binary_xor
       ; "binary_nand" >:: test_binary_nand
       ; "binary_nor" >:: test_binary_nor
       ; "binary_nxor" >:: test_binary_nxor
       ; "not_precedence" >:: test_not_has_higher_precedence
       ; "and_precedence" >:: test_and_has_higher_precedence_than_or
       ; "left_associativity" >:: test_left_associativity
       ; "nested_parentheses" >:: test_nested_parentheses
       ]
;;

let () = run_test_tt_main test_parser
