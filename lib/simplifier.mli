type t

type expression =
  | SOP of Sum_of_products.t
  | One

val empty : variable_count:int -> t
val return : t -> expression
val add_simple_term : Term.t -> t -> t
