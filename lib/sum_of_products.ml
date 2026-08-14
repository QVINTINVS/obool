open Term

type trie =
  | Empty
  (* A stored product term.
   Remaining variables are implicitly absent, avoiding unnecessary nodes. *)
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

let empty ~variable_count = { variable_count; root = Empty }

let branches_of = function
  | Node n -> n
  | Empty | Terminal -> { absent = Empty; present = Empty }
;;

let child { absent; present } = function
  | Present -> present
  | Absent -> absent
;;

let with_child branches occurrence updated_trie =
  match occurrence with
  | Present -> Node { branches with present = updated_trie }
  | Absent -> Node { branches with absent = updated_trie }
;;

let rec insert_term ?(depth = 0) variable_count term trie =
  if depth = variable_count * 2 || all_zero_from term depth
  then Terminal
  else (
    let branches = branches_of trie in
    let occurrence = literal_occurrence term depth in
    child branches occurrence
    |> insert_term ~depth:(succ depth) variable_count term
    |> with_child branches occurrence)
;;

let add_term term sop =
  match normalize term with
  | Zero -> sop
  | Term term ->
    { sop with root = insert_term sop.variable_count term sop.root }
;;
