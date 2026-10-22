type t =
  { positive_bits : Bitv.t
  ; negated_bits : Bitv.t
  }

type normalized =
  | Zero
  | Term of t

type variable =
  | Absent
  | Present

let of_lsb_ints ?(variable_count = Sys.int_size - 1) positive negated =
  { positive_bits = Bitv.(of_int_us positive |> get |> init variable_count)
  ; negated_bits = Bitv.(of_int_us negated |> get |> init variable_count)
  }
;;

let term2 = of_lsb_ints ~variable_count:2
let term4 = of_lsb_ints ~variable_count:4

let truncate variable_count term =
  { positive_bits = Bitv.sub term.positive_bits 0 variable_count
  ; negated_bits = Bitv.sub term.negated_bits 0 variable_count
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

let variables term =
  Seq.append
    (Seq.init (Bitv.length term.positive_bits) (Bitv.get term.positive_bits))
    (Seq.init (Bitv.length term.negated_bits) (Bitv.get term.negated_bits))
  |> Seq.map (fun var -> if var then Present else Absent)
;;

let int_of_bits bits =
  Seq.fold_lefti (fun acc i bit -> acc lor (bit lsl i)) 0 bits
;;

let of_variables_list ~variable_count variables =
  let bits_seq =
    variables
    |> List.to_seq
    |> Seq.map (function
      | Present -> 1
      | Absent -> 0)
  in
  of_lsb_ints
    ~variable_count
    (Seq.take variable_count bits_seq |> int_of_bits)
    (Seq.drop variable_count bits_seq |> int_of_bits)
;;
