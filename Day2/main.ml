let print_int_array(arr: (int array)): unit =
  let len = Array.length arr in
  let len_m1 = len - 1 in
  if Array.length arr == 0 then
    print_string "[]\n"
  else
    let print_i(index: int)(elem: int): unit =
      let postfix = match (len_m1 - index) with 
        | 0 -> ""
        | _ -> ", "
      in Printf.printf("%d%s") elem postfix
    in
    print_string "[";
    Array.iteri (fun i e -> print_i i e) arr;
    print_string "]\n"

let execop(position: int) (opcode: int) (code: (int array)): (int array) =
  let opint: (int -> int -> int) = if opcode == 1 then (fun a b -> a + b) else (fun a b -> a * b) in
  let in1 = Array.get code (position + 1) in
  let in1_val = Array.get code in1 in
  let in2 = Array.get code (position + 2) in
  let in2_val = Array.get code in2 in
  let out = Array.get code (position + 3) in
  Printf.printf("Executing at %d (opcode: %d, in1: %d -> %d, in2: %d -> %d, out: %d)\n") position opcode in1 in1_val in2 in2_val out;
  Array.set code out (opint in1_val in2_val);
  code

let rec exec(position: int) (code: (int array)): (int array) =
  Printf.printf "-- Executing on:\n";
  print_int_array code;

  let opcode = Array.get code position in
  match opcode with
   | 99 -> Printf.printf("Got 99, exiting.\n");code
   | 1 | 2 -> exec (position + 4) (execop position opcode code)
   | _ -> failwith ("Invalid opcode " ^ (string_of_int opcode))

let () =
  let result = exec 0 (Array.of_list [1;0;0;0;99]) in
  Printf.printf "-- Result:\n";
  print_int_array result
