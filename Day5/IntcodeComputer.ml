open Commons

(* instructions *)

type instruction = {
  opcode: int;
  param_count: int;
}

let instruction_add: instruction = { 
  opcode = 1; 
  param_count = 3;
}

let instruction_mul: instruction = { 
  opcode = 2; 
  param_count = 3;
}

let instruction_set: instruction = {
  opcode = 3;
  param_count = 1;
}

let instruction_print: instruction = {
  opcode = 4;
  param_count = 1;
}

let instruction_halt: instruction = {
  opcode = 99;
  param_count = 0;
}

let instruction_of_opcode(opcode: int): instruction =
  match opcode with
  | 0 -> instruction_halt
  | 1 -> instruction_add
  | 2 -> instruction_mul
  | 3 -> instruction_set
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

let read_params(arr: int array) (from: int) (n: int): int array =
  let params = Array.make n 0 in
  let rec read_em (i: int): int array =
    match i with
    | _ when i == n -> params
    | _ -> Array.set params i (Array.get arr (from + i)); read_em (i+1)
  in read_em n

let _read_execution_simple (arr: int array) (position: int) : execution =
  let opcode = Array.get arr position in
  let instruction = instruction_of_opcode opcode in
  let params = read_params arr (position + 1) instruction.param_count in
  let params_and_mode = Array.map (fun p -> (p, 0)) params in
  { 
    array = arr;
    position = position;
    instruction = instruction;
    params_and_mode = Array.to_list params_and_mode;
  }

let _read_execution_with_modes (arr: int array) (position: int) : execution =
  let operation = Array.get arr position in
  let operation_str = string_of_int operation in
  let opcode_str = String.sub operation_str (String.length operation_str - 2) (String.length operation_str) in
  let instruction = instruction_of_opcode (int_of_string opcode_str) in
  let param_modes_str = String.sub operation_str 0 (String.length operation_str - 2) in
  let params = read_params arr (position + 1) instruction.param_count in
  let params_modes = List.map (fun c -> int_of_string (Char.escaped c)) (Basics.chars_of_string param_modes_str) in
  let params_and_mode = List.combine (Array.to_list params) params_modes in
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

let execute(arr: int array): int array =
  let rec exec_step(arr: int array) (position: int): int array =
    let execution = read_execution arr position in
    match execution.instruction with
    |

let () = print_endline "IntCode computer primitives"
