open OUnit2
open Boolean_expression_simplifier
open Parser

let test_single_operand _ =
  assert_equal
    [ OPERAND "ABCDEFGHIJKLMNOPQSRTUVWXYZ" ]
    (Tokenizer.tokenize "ABCDEFGHIJKLMNOPQSRTUVWXYZ")
;;

let test_simple_binary_expression _ =
  assert_equal
    [ OPERAND "A"; NAND; OPERAND "BA"; AND; OPERAND "C" ]
    (Tokenizer.tokenize "A nand BA and C")
;;

let test_or_operator _ =
  assert_equal [ OPERAND "A"; OR; OPERAND "B" ] (Tokenizer.tokenize "A or B")
;;

let test_and_operator _ =
  assert_equal [ OPERAND "A"; AND; OPERAND "B" ] (Tokenizer.tokenize "A and B")
;;

let test_xor_operator _ =
  assert_equal [ OPERAND "A"; XOR; OPERAND "B" ] (Tokenizer.tokenize "A xor B")
;;

let test_nor_operator _ =
  assert_equal [ OPERAND "A"; NOR; OPERAND "B" ] (Tokenizer.tokenize "A nor B")
;;

let test_nand_operator _ =
  assert_equal
    [ OPERAND "A"; NAND; OPERAND "B" ]
    (Tokenizer.tokenize "A nand B")
;;

let test_nxor_operator _ =
  assert_equal
    [ OPERAND "A"; NXOR; OPERAND "B" ]
    (Tokenizer.tokenize "A nxor B")
;;

let test_unary_not _ =
  assert_equal
    [ NOT; OPERAND "ACH"; NXOR; OPERAND "A"; NOT; OPERAND "B" ]
    (Tokenizer.tokenize "'ACH nxor A'B")
;;

let test_parenthesized_expression _ =
  assert_equal
    [ LPAREN; OPERAND "A"; OR; OPERAND "B"; RPAREN; AND; OPERAND "C" ]
    (Tokenizer.tokenize "(A or B) and C")
;;

let test_nested_parenthesized_expression _ =
  assert_equal
    [ LPAREN
    ; OPERAND "AG"
    ; NOR
    ; LPAREN
    ; NOT
    ; LPAREN
    ; OPERAND "K"
    ; AND
    ; NOT
    ; OPERAND "Z"
    ; RPAREN
    ; RPAREN
    ; NXOR
    ; LPAREN
    ; OPERAND "OPD"
    ; NOR
    ; OPERAND "B"
    ; OR
    ; OPERAND "A"
    ; RPAREN
    ; RPAREN
    ]
    (Tokenizer.tokenize "(AG nor ('(K and 'Z)) nxor (OPD nor B or A))")
;;

let test_invalid_symbols _ =
  try
    ignore (Tokenizer.tokenize "P + (IO and (A' + B) + C)J");
    assert_failure "Expected a lexical error"
  with
  | Lexer.Syntax_error _ -> ()
;;

let tokenizer_tests =
  "tokenizer"
  >::: [ "single_operand" >:: test_single_operand
       ; "simple_binary_expression" >:: test_simple_binary_expression
       ; "operator_or" >:: test_or_operator
       ; "operator_and" >:: test_and_operator
       ; "operator_xor" >:: test_xor_operator
       ; "operator_nor" >:: test_nor_operator
       ; "operator_nand" >:: test_nand_operator
       ; "operator_nxor" >:: test_nxor_operator
       ; "unary_not" >:: test_unary_not
       ; "parenthesized_expression" >:: test_parenthesized_expression
       ; "nested_parenthesized_expression"
         >:: test_nested_parenthesized_expression
       ; "invalid_symbols" >:: test_invalid_symbols
       ]
;;

let () = run_test_tt_main tokenizer_tests
