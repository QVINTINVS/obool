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
  | Terminal -> { absent = Terminal; present = Empty }
  | Empty -> { absent = Empty; present = Empty }
;;

let child { absent; present } = function
  | Present -> present
  | Absent -> absent
;;

let is_leaf = function
  | Empty | Terminal -> true
  | _ -> false
;;

let with_child branches occurrence updated_trie =
  match occurrence with
  | Present -> Node { branches with present = updated_trie }
  | Absent -> Node { branches with absent = updated_trie }
;;

let rec insert_term ?(depth = 0) variable_count term trie =
  if depth = variable_count * 2 || (is_leaf trie && all_zero_from term depth)
  then Terminal
  else (
    let branches = branches_of trie in
    let occurrence = literal_occurrence term depth in
    child branches occurrence
    |> insert_term ~depth:(succ depth) variable_count term
    |> with_child branches occurrence)
;;

let add_term term sop =
  let { variable_count; root } = sop in
  let term = truncate variable_count term in
  let updated_root = insert_term variable_count term root in
  { sop with root = updated_root }
;;
