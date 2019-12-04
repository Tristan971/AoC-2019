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

let has_dupe_strictly_2 (digits : char list) : bool =
  let rec find_2_strict (rem : char list) (previous : char) (count : int) =
    match rem with
    | [] -> count == 2
    | a :: t when a != previous ->
        if count == 2 then true else find_2_strict t a 1
    | a :: t when a == previous -> find_2_strict t a (count + 1)
    | _ -> failwith "Unexpected"
  in
  find_2_strict digits 'x' 0

let is_valid_p1 (n : int) : bool =
  let digits = List.of_seq (String.to_seq (string_of_int n)) in
  has_dupe digits && is_increasing digits

let is_valid_p2 (n : int) : bool =
  let digits = List.of_seq (String.to_seq (string_of_int n)) in
  has_dupe_strictly_2 digits && is_increasing digits

let count_in_range (low : int) (high : int) (validator : int -> bool) : unit =
  let rec try_from (tried : int) (valids : int list) : int list =
    match tried with
    | n when n >= high -> valids
    | n when not (validator n) -> try_from (n + 1) valids
    | n -> try_from (n + 1) (n :: valids)
  in
  let valids = try_from low [] in
  print_endline ("Valid numbers count: " ^ string_of_int (List.length valids))

let assert_validity_p2 (n : int) (expect : bool) : unit =
  if is_valid_p2 n != expect then
    Printf.printf "Expected %d to have validity %s !" n (string_of_bool expect)

let samples_2 () =
  assert_validity_p2 112233 true;
  assert_validity_p2 111122 true;
  assert_validity_p2 221111 false;
  assert_validity_p2 123444 false;
  assert_validity_p2 666669 false;
  assert_validity_p2 444449 false

let () =
  samples_2 ();
  count_in_range 147981 691423 is_valid_p1;
  count_in_range 147981 691423 is_valid_p2
