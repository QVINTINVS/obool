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
  | Node _ -> false
;;

let with_child branches occurrence updated_trie =
  match occurrence with
  | Present -> Node { branches with present = updated_trie }
  | Absent -> Node { branches with absent = updated_trie }
;;

let all_absent variables = List.for_all (( = ) Absent) variables

let rec insert_term variables trie =
  match variables with
  | [] -> Terminal
  | _ when is_leaf trie && all_absent variables -> Terminal
  | variable :: rest ->
    let branches = branches_of trie in
    let updated_child = child branches variable |> insert_term rest in
    with_child branches variable updated_child
;;

let add_term term sop =
  let { variable_count; root } = sop in
  let term = Term.truncate variable_count term in
  let variables = variables term |> List.of_seq in
  let updated_root = insert_term variables root in
  if all_absent variables then sop else { sop with root = updated_root }
;;

let absorption_branches branches = function
  | Present -> [ Present, branches.present; Absent, branches.absent ]
  | Absent -> [ Absent, branches.absent ]
;;

let rec find_absorbing_path variables trie =
  match trie, variables with
  | Terminal, rest -> Some (List.map (fun _ -> Absent) rest)
  | Empty, _ | _, [] -> None
  | Node branches, variable :: rest ->
    absorption_branches branches variable
    |> List.find_map (fun (found_occurrence, subtree) ->
      find_absorbing_path rest subtree
      |> Option.map (fun found -> found_occurrence :: found))
;;

let get_first_term_that_absorves term sop =
  let { variable_count; root } = sop in
  let term = Term.truncate variable_count term in
  let variables = variables term |> List.of_seq in
  find_absorbing_path variables root
  |> Option.map (of_variables_list ~variable_count)
;;
