open Commons

let print_int_array(arr: (int array))(from: (int)): unit =
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
    Array.iteri (fun i e -> if i >= from then print_i i e) arr;
    print_string "]\n"

let execop(position: int) (opcode: int) (code: (int array)): (int array) =
  let opint: (int -> int -> int) = if opcode == 1 then (fun a b -> a + b) else (fun a b -> a * b) in
  let in1 = Array.get code (position + 1) in
  let in1_val = Array.get code in1 in
  let in2 = Array.get code (position + 2) in
  let in2_val = Array.get code in2 in
  let out = Array.get code (position + 3) in
  (* Printf.printf("Executing at %d (opcode: %d, in1: %d -> %d, in2: %d -> %d, out: %d)\n") position opcode in1 in1_val in2 in2_val out; *)
  Array.set code out (opint in1_val in2_val);
  code

type noun_verb = {
  noun: int;
  verb: int;
}

let rec exec(position: int) (code: (int array)): (int array) =
  (*
  Printf.printf "\n-- Executing on position %d:\n" position;
  print_int_array code position;
  *)

  let opcode = Array.get code position in
  match opcode with
   | 99 -> code
   | 1 | 2 -> exec (position + 4) (execop position opcode code)
   | _ -> failwith ("Invalid opcode " ^ (string_of_int opcode))

let split_as_ints(input: string): (int array) =
  let input_split = String.split_on_char ',' input in
  let input_ints = List.map int_of_string input_split in
  Array.of_list input_ints

let exec_for(input: string) =
  let result = exec 0 (split_as_ints input) in
  Printf.printf "\n-- Result:\n";
  print_int_array result 0;
  Array.to_list result

let samples(): unit = 
  let _ = exec_for("1,0,0,0,99") in
  let _ = exec_for("2,3,0,3,99") in
  let _ = exec_for("2,4,4,5,99,0") in
  let _ = exec_for("1,1,1,4,99,5,6,0,99") in
  print_string "Done running samples\n"


let exec_with(input: string)(params: noun_verb): (int array) =
  let input_arr: (int array) = split_as_ints input in
  Array.set input_arr 1 params.noun;
  Array.set input_arr 2 params.verb;
  exec 0 input_arr

let read_input(): string = 
  let input = IOUtils.read_all_lines "./Day2/input" in
  match input with
    | h::_ -> h
    | _ -> failwith "Invalid input given!"

let part1(): unit =
  print_string "\n-- PART 1 --\n";
  
  let input = read_input() in
  
  let params: noun_verb = {
    noun=12; 
    verb=2
  } in 
  print_int_array (exec_with input params) 0

let part2(): unit =
  print_string "\n-- PART 2 --\n";
  
  let input = read_input() in
  
  let next(params: noun_verb): noun_verb = 
    let next_params =
      if params.noun < 99 then { noun = (params.noun + 1); verb = params.verb }
      else if params.verb < 99 then { noun = 0; verb = (params.verb + 1) }
      else  failwith("No next after noun at " ^ string_of_int params.noun ^ " and verb at " ^ string_of_int params.verb)
    in (*Printf.printf "-> Next params: { %d, %d }\n" next_params.noun next_params.verb;*) next_params
  in

  let rec search_em(params: noun_verb) =
    let result = exec_with input params in
    let output = Array.get result 0 in
    match output with
      | 19690720 -> params
      | _ -> search_em (next params)
  in
  let solution = search_em({noun=0; verb=0}) in
  Printf.printf "Solution was: { noun=%d; verb=%d }\n" solution.noun solution.verb;
  Printf.printf "Yielding secret: %d\n" (solution.noun * 100 + solution.verb)


let () =
  part1();
  part2();
  print_string "Over and out.\n"
