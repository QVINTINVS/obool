type 'operand expression =
  | Operand of 'operand
  | Not of 'operand expression
  | And of 'operand expression * 'operand expression
  | Or of 'operand expression * 'operand expression
  | Xor of 'operand expression * 'operand expression
  | Nand of 'operand expression * 'operand expression
  | Nor of 'operand expression * 'operand expression
  | Nxor of 'operand expression * 'operand expression
