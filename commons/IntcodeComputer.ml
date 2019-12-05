(* instructions *)

type instruction = {
  opcode: int;
  param_count: int;
}

let instruction_add: instruction = { 
  opcode = 1; 
  param_count = 3;
}
let execution_add (arr: int array) (params_l: (int * int) list) : int =
  let params_a = Array.of_list params_l in
  let (m1, i1) = Array.get params_a 0 in
  let (m2, i2) = Array.get params_a 1 in
  let (_, io) = Array.get params_a 0 in
  let input1 = if m1 == 0 then Array.get arr i1 else i1 in
  let input2 = if m2 == 0 then Array.get arr i2 else i2 in
  let output = io in
  Array.set arr output (input1 + input2);
  4

let instruction_mul: instruction = { 
  opcode = 2; 
  param_count = 3;
}

let execution_mul (arr: int array) (params_l: (int * int) list) : int =
  let params_a = Array.of_list params_l in
  let (m1, i1) = Array.get params_a 0 in
  let (m2, i2) = Array.get params_a 1 in
  let (_, io) = Array.get params_a 0 in
  let input1 = if m1 == 0 then Array.get arr i1 else i1 in
  let input2 = if m2 == 0 then Array.get arr i2 else i2 in
  let output = io in
  Array.set arr output (input1 * input2);
  4

let instruction_input: instruction = {
  opcode = 3;
  param_count = 1;
}

let execution_input (arr: int array) (params_l: (int * int) list) : int =
  let (_, output) = List.hd params_l in
  let input = read_int() in
  Array.set arr output input;
  2

let instruction_print: instruction = {
  opcode = 4;
  param_count = 1;
}

let execution_print (arr: int array) (params_l: (int * int) list): int =
  let (mi, pi) = List.hd params_l in
  let input = if mi == 0 then Array.get arr pi else pi in
  Printf.printf "\n[ OUTPUT ] => %d\n" input;
  2

let instruction_halt: instruction = {
  opcode = 99;
  param_count = 0;
}

let instruction_of_opcode(opcode: int): instruction =
  match opcode with
  | 0 -> instruction_halt
  | 1 -> instruction_add
  | 2 -> instruction_mul
  | 3 -> instruction_input
  | 4 -> instruction_print
  | _ -> failwith("Unknown instruction code: " ^ string_of_int opcode)

(* execution context *)

type execution = {
  (* positional *)
  array: int array;
  position: int;
  instruction: instruction;

  (* functional *)
  params_and_mode: (int * int) list;
}

(* due to recursive parsing, param 0 will have index 0, and this is meant to be read left-to-right *)
let read_params(arr: int array) (from: int) (to_i: int): int list =
  Printf.printf "Reading params from %d to %d (excl.)\n" from to_i;
  let rec read (i: int) (read_p: int list): int list =
    match i with
    | _ when i == to_i -> read_p
    | _ -> read (i + 1) ((Array.get arr i)::read_p)
  in read from []

let _read_execution_simple (arr: int array) (position: int) : execution =
  let opcode = Array.get arr position in
  let instruction = instruction_of_opcode opcode in
  let params_start_pos = position + 1 in
  let params = read_params arr params_start_pos (params_start_pos + instruction.param_count) in
  let params_and_mode = List.map (fun p -> (p, 0)) params in
  {
    array = arr;
    position = position;
    instruction = instruction;
    params_and_mode = params_and_mode;
  }

let _read_execution_with_modes (arr: int array) (position: int) : execution =
  let operation = Array.get arr position in
  let operation_str = string_of_int operation in
  let opcode_str = String.sub operation_str (String.length operation_str - 2) (String.length operation_str) in
  let instruction = instruction_of_opcode (int_of_string opcode_str) in
  let param_modes_str = String.sub operation_str 0 (String.length operation_str - 2) in
  let params_start_pos = position + 1 in
  let params = read_params arr params_start_pos (params_start_pos + instruction.param_count) in
  let params_modes = List.map (fun c -> int_of_string (Char.escaped c)) (List.of_seq (String.to_seq param_modes_str)) in
  let params_and_mode = List.combine params params_modes in
  {
    array = arr;
    position = position;
    instruction = instruction;
    params_and_mode = params_and_mode;
  }

let read_execution (arr: int array) (position: int) : execution =
  let opcode = Array.get arr position in
  match opcode with
    | _ when opcode < 100 -> _read_execution_simple arr position
    | _ -> _read_execution_with_modes arr position


(* entrypoints *)

let apply_execution_and_get_next_position(e: execution): int =
  Printf.printf "Executing @ %d with %d and params: [ " e.position e.instruction.opcode;
  List.iter (fun (i1, i2) -> Printf.printf " %d|%d " i1 i2) e.params_and_mode;
  Printf.printf " ]\n";
  match e.instruction.opcode with
  | 0 -> failwith "Cannot execute a halt!!!"
  | 1 -> execution_add e.array e.params_and_mode
  | 2 -> execution_mul e.array e.params_and_mode
  | 3 -> execution_input e.array e.params_and_mode
  | 4 -> execution_print e.array e.params_and_mode
  | _ -> failwith ("Unknown opcode! " ^ (string_of_int e.instruction.opcode))

let execute(arr: int array): int array =
  let rec execute_from (position: int): int array =
    let execution = read_execution arr position in
    match execution.instruction with
    | instruction when instruction == instruction_halt -> arr
    | __ -> execute_from (apply_execution_and_get_next_position execution)
  in execute_from 0

let () = print_endline "Loaded IntCode virtual machine"
