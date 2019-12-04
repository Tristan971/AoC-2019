module CharSet = Set.Make (Char)

let is_increasing (digits : char list) : bool =
  let sorted = List.sort Char.compare digits in
  sorted = digits

let has_dupe (digits : char list) : bool =
  let rec is_duped_with_next (remaining : char list) : bool =
    match remaining with
    | c1 :: c2 :: _ when c1 = c2 -> true
    | _ :: t -> is_duped_with_next t
    | _ -> false
  in
  is_duped_with_next digits

let count_in_range (low : int) (high : int) : unit =
  let is_valid (n : int) : bool =
    let digits = List.of_seq (String.to_seq (string_of_int n)) in
    has_dupe digits && is_increasing digits
  in

  let rec try_from (tried : int) (valids: int list) : int list =
    match tried with
    | n when n >= high -> valids
    | n when not (is_valid n) -> try_from (n + 1) valids
    | n -> try_from (n + 1) (n::valids)
  in
  let valids = try_from low [] in
  print_endline ("Valid numbers: " ^ (string_of_int (List.length valids)))

let () =
  count_in_range 147981 691423
