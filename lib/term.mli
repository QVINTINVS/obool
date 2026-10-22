type t

type normalized =
  | Zero
  | Term of t

type variable =
  | Absent
  | Present

val of_lsb_ints : ?variable_count:int -> int -> int -> t
val term2 : int -> int -> t
val term4 : int -> int -> t
val truncate : int -> t -> t
val common_factor : t -> t -> t
val normalize : t -> normalized
val literal_occurrence : t -> int -> variable
val all_zero_from : t -> int -> bool
val variables : t -> variable Seq.t
val of_variables_list : variable_count:int -> variable list -> t
