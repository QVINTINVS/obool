type trie =
  | Empty
  | Terminal
  | Node of node

and node =
  { absent : trie
  ; present : trie
  }

type t =
  { variable_count : int
  ; root : trie
  }

val empty : variable_count:int -> t
val add_term : Term.t -> t -> t
val get_first_term_that_absorves : Term.t -> t -> Term.t option
