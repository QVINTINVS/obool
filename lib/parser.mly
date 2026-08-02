%{
open Ast
%}

%token <string> OPERAND

%token AND OR XOR NAND NOR NXOR
%token NOT

%token LPAREN RPAREN
%token EOF

%left OR NOR XOR NXOR
%left AND NAND
%right NOT

%start main
%type <string Ast.expression> main
%type <string Ast.expression> expression

%%

main:
    expression EOF
      { $1 }

expression:
    OPERAND
      { Operand $1 }

  | LPAREN expression RPAREN
      { $2 }

  | NOT expression
      { Not $2 }

  | expression AND expression
      { And ($1, $3) }

  | expression OR expression
      { Or ($1, $3) }

  | expression XOR expression
      { Xor ($1, $3) }

  | expression NAND expression
      { Nand ($1, $3) }

  | expression NOR expression
      { Nor ($1, $3) }

  | expression NXOR expression
      { Nxor ($1, $3) }
;
