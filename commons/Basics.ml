let digits_of_int (number : int) : int list =
  let rec get_digits (n : int) (digits : int list) : int list =
    match n with
    | _ when n < 10 -> n :: digits
    | _ -> get_digits (n / 10) ((n mod 10) :: digits)
  in
  List.rev (get_digits number [])

let chars_of_string (str : string) : char list = List.of_seq (String.to_seq str)

let combine_arrays (a1 : 'a array) (a2 : 'b array) : ('a * 'b) array =
  assert (Array.length a1 == Array.length a2);
  Array.of_list (List.combine (Array.to_list a1) (Array.to_list a2))

let print_int_array (arr : int array) (from : int) : unit =
  let len = Array.length arr in
  let len_m1 = len - 1 in
  if Array.length arr == 0 then print_string "[]\n"
  else
    let print_i (index : int) (elem : int) : unit =
      let postfix = match len_m1 - index with 0 -> "" | _ -> ", " in
      Printf.printf "%d%s" elem postfix
    in
    print_string "[";
    Array.iteri (fun i e -> if i >= from then print_i i e) arr;
    print_string "]\n"

(* 1 (length of 1) -> 4-length -> [000]1 *)
let pad (input : string) (length_wanted : int) (pad_character : char) : string =
  let needed_count = length_wanted - String.length input in
  if needed_count <= 0 then input
  else
    let rec append (cur : string) (left : int) : string =
      match left with
      | 0 -> cur
      | _ -> append (Char.escaped pad_character ^ cur) (left - 1)
    in
    let padding = append "" needed_count in
    padding ^ input

let rec do_x_times (times : int) (call : _ -> unit) : unit =
  match times <= 0 with
  | true -> print_string ""
  | false ->
      call ();
      do_x_times (times - 1) call

module StringMap = Map.Make (String)

let group_by (mapping : 'a -> string) (from : 'a list) : 'a list StringMap.t =
  let empty_opt_to_list_with (opt : 'a list option) (added : 'a) :
      'a list option =
    let res = match opt with Some a -> added :: a | None -> [ added ] in
    Some res
  in
  let accumulate (next : 'a) (map : 'a list StringMap.t) : 'a list StringMap.t =
    let key = mapping next in
    StringMap.update key (fun prev_o -> empty_opt_to_list_with prev_o next) map
  in
  List.fold_left (fun sofar next -> accumulate next sofar) StringMap.empty from

let split_as_ints (input : string) : int array =
  let input_split = String.split_on_char ',' input in
  let input_ints = List.map int_of_string input_split in
  Array.of_list input_ints

(* Permutations function by: https://gist.github.com/MassD/fa79de3a5ee88c9c5a8e *)
let ins_all_positions x l =
  let rec aux prev acc = function
    | [] -> (prev @ [ x ]) :: acc |> List.rev
    | hd :: tl as l -> aux (prev @ [ hd ]) ((prev @ [ x ] @ l) :: acc) tl
  in
  aux [] [] l

let rec permutations = function
  | [] -> []
  | [ x ] -> [ [ x ] ] (* we must specify this edge case *)
  | x :: xs ->
      List.fold_left
        (fun acc p -> acc @ ins_all_positions x p)
        [] (permutations xs)
