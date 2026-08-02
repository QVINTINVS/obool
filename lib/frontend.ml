open Parser
open Lexing

let parse expression =
  expression |> Lexing.from_string |> Parser.main Lexer.token
;;
