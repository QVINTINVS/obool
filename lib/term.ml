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

let of_lsb_ints ?(variable_count = Sys.int_size - 1) positive negated =
  { positive_bits = Bitv.(of_int_us positive |> get |> init variable_count)
  ; negated_bits = Bitv.(of_int_us negated |> get |> init variable_count)
  }
;;

let common_factor left right =
  { positive_bits = Bitv.bw_and left.positive_bits right.positive_bits
  ; negated_bits = Bitv.bw_and left.negated_bits right.negated_bits
  }
;;

let is_contradictory term =
  Bitv.(bw_and term.positive_bits term.negated_bits |> all_zeros |> not)
;;

let is_empty term =
  Bitv.(bw_or term.positive_bits term.negated_bits |> all_zeros)
;;

let normalize term =
  if is_contradictory term || is_empty term then Zero else Term term
;;

let literal_occurrence term position =
  let bits = Bitv.append term.positive_bits term.negated_bits in
  let clamped_position = Bitv.length bits - 1 |> min position |> max 0 in
  if Bitv.get bits clamped_position then Present else Absent
;;

let all_zero_from term position =
  Bitv.(
    append term.positive_bits term.negated_bits
    |> fun bits -> sub bits position (length bits - position) |> all_zeros)
;;
