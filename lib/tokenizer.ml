open Lexer

let tokenize input =
  let lexbuf = Lexing.from_string input in
  let rec loop acc =
    match Lexer.token lexbuf with
    | Parser.EOF -> List.rev acc
    | token -> loop (token :: acc)
  in
  loop []
;;
