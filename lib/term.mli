type t =
  { positive_bits : Bitv.t
  ; negated_bits : Bitv.t
  }

type normalized =
  | Zero
  | Term of t

type occurrence =
  | Absent
  | Present

val of_lsb_ints : int -> int -> t
val common_factor : t -> t -> t
val normalize : t -> normalized
val positive_variable_at : t -> int -> occurrence
val negated_variable_at : t -> int -> occurrence
