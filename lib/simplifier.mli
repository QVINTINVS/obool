type t

type value =
  | SOP of Sum_of_products.t
  | One

val empty : variable_count:int -> t
val return : t -> value
val add_simple_term : Term.t -> t -> t
