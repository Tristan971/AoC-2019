let digits_of_int (number: int): (int list) =
  let rec get_digits (n: int) (digits: int list): int list =
    match n with
    | _ when n < 10 -> n::digits
    | _ -> get_digits (n / 10) ((n mod 10)::digits)
  in List.rev (get_digits number [])

let chars_of_string (str: string): char list =
  List.of_seq (String.to_seq str)
 
let combine_arrays (a1: 'a array) (a2: 'b array): ('a * 'b) array =
  assert(Array.length a1 == Array.length a2);
  Array.of_list (List.combine (Array.to_list a1) (Array.to_list a2))
