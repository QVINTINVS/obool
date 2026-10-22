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

let rec insert_term variables trie =
  match variables with
  | [] -> Terminal
  | _ when is_leaf trie && List.for_all (( = ) Absent) variables -> Terminal
  | variable :: rest ->
    let branches = branches_of trie in
    let updated_child = child branches variable |> insert_term rest in
    with_child branches variable updated_child
;;

let add_term term sop =
  let { variable_count; root } = sop in
  let term = truncate variable_count term in
  let variables = variables term |> List.of_seq in
  let updated_root = insert_term variables root in
  { sop with root = updated_root }
;;
