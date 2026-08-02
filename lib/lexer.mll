{
open Parser

exception Syntax_error of string
}

rule token = parse
| [' ' '\t' '\n']     { token lexbuf }

| '('                 { LPAREN }
| ')'                 { RPAREN }

| '\''                { NOT }

| "and"               { AND }
| "or"                { OR }
| "xor"               { XOR }
| "nand"              { NAND }
| "nor"               { NOR }
| "nxor"              { NXOR }

| ['A'-'Z']+ as id    { OPERAND id }

| eof                 { EOF }

| _                   { raise (Syntax_error "invalid character") }
