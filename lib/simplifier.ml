open Term
open Sum_of_products

type expression =
  | SOP of Sum_of_products.t
  | One

type t = Simplified of expression

type state =
  { term : Term.t
  ; sop : expression
  }

type simplification_step =
  | Halt
  | Continue of state

let empty ~variable_count = Simplified (SOP (empty ~variable_count))

let return = function
  | Simplified expression -> expression
;;

let add_expression term = function
  | One -> One
  | SOP sop -> SOP (add_term term sop)
;;

let apply_normalization state =
  match normalize state.term with
  | Zero -> Halt
  | Term term -> Continue { state with term }
;;

let apply_absorved_condition state =
  match state.sop with
  | One -> Halt
  | SOP sop ->
    get_first_term_that_absorves state.term sop
    |> Option.fold ~none:(Continue state) ~some:(fun _ -> Halt)
;;

let bind_step rule = function
  | Halt -> Halt
  | Continue state -> rule state
;;

let unwrap_step step state =
  match step with
  | Halt -> state.sop
  | Continue updated_state ->
    add_expression updated_state.term updated_state.sop
;;

let apply_simplification_rules state =
  Continue state
  |> bind_step apply_normalization
  |> bind_step apply_absorved_condition
  |> fun step -> unwrap_step step state
;;

let add_simple_term term (Simplified expression) =
  match expression with
  | One -> Simplified One
  | SOP _ -> Simplified (apply_simplification_rules { term; sop = expression })
;;
