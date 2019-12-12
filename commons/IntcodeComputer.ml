(* instructions *)

type instruction = { opcode : int; param_count : int }

type execution = {
  (* positional *)
  array : int array;
  position : int;
  instruction : instruction;
  (* functional *)
  params_and_mode : (int * int) list;
}

type execution_result = { next_position : int; output : int option }

let exec_result_of_next_pos (position : int) : execution_result =
  { next_position = position; output = None }

let instruction_add : instruction = { opcode = 1; param_count = 3 }

let execution_add (e : execution) : execution_result =
  let params_a = Array.of_list e.params_and_mode in
  let m1, i1 = params_a.(0) in
  let m2, i2 = params_a.(1) in
  let _, io = params_a.(2) in
  let input1 = if m1 == 0 then e.array.(i1) else i1 in
  let input2 = if m2 == 0 then e.array.(i2) else i2 in
  let output = io in
  (* print_string "Arr is now: ";
  Basics.print_int_array arr 0;
  Printf.printf "%d + %d -> &%d\n" input1 input2 output; *)
  e.array.(output) <- input1 + input2;
  exec_result_of_next_pos (e.position + 4)

let instruction_mul : instruction = { opcode = 2; param_count = 3 }

let execution_mul (e : execution) : execution_result =
  let params_a = Array.of_list e.params_and_mode in
  let m1, i1 = params_a.(0) in
  let m2, i2 = params_a.(1) in
  let _, io = params_a.(2) in
  let input1 = if m1 == 0 then e.array.(i1) else i1 in
  let input2 = if m2 == 0 then e.array.(i2) else i2 in
  let output = io in
  (* print_string "Arr is now: ";
  Basics.print_int_array arr 0;
  Printf.printf "%d * %d -> &%d\n" input1 input2 output; *)
  e.array.(output) <- input1 * input2;
  exec_result_of_next_pos (e.position + 4)

let instruction_input : instruction = { opcode = 3; param_count = 1 }

let execution_input (input : int) (e : execution) : execution_result =
  let _, output = List.hd e.params_and_mode in
  (* print_string "INPUT REQUEST: "; *)
  e.array.(output) <- input;
  exec_result_of_next_pos (e.position + 2)

let instruction_print : instruction = { opcode = 4; param_count = 1 }

let execution_print (e : execution) : execution_result =
  let mi, pi = List.hd e.params_and_mode in
  let input = if mi == 0 then e.array.(pi) else pi in
  Printf.printf "Output => %d\n" input;
  { next_position = e.position + 2; output = Some input }

let instruction_jmp_if_true : instruction = { opcode = 5; param_count = 2 }

let execution_jmp_if_true (e : execution) : execution_result =
  let mv, v = List.hd e.params_and_mode in
  let mt, t = List.nth e.params_and_mode 1 in
  let tested = if mv = 0 then e.array.(v) else v in
  let instruction_ptr = if mt = 0 then e.array.(t) else t in
  if tested != 0 then exec_result_of_next_pos instruction_ptr
  else exec_result_of_next_pos (e.position + 3)

let instruction_jmp_if_false : instruction = { opcode = 6; param_count = 2 }

let execution_jmp_if_false (e : execution) : execution_result =
  let mv, v = List.hd e.params_and_mode in
  let mt, t = List.nth e.params_and_mode 1 in
  let tested = if mv = 0 then e.array.(v) else v in
  let instruction_ptr = if mt = 0 then e.array.(t) else t in
  if tested == 0 then exec_result_of_next_pos instruction_ptr
  else exec_result_of_next_pos (e.position + 3)

let instruction_less_than : instruction = { opcode = 7; param_count = 3 }

let execution_less_than (e : execution) : execution_result =
  let params_a = Array.of_list e.params_and_mode in
  let m1, i1 = params_a.(0) in
  let m2, i2 = params_a.(1) in
  let _, io = params_a.(2) in
  let input1 = if m1 == 0 then e.array.(i1) else i1 in
  let input2 = if m2 == 0 then e.array.(i2) else i2 in
  let output = io in
  let result = if input1 < input2 then 1 else 0 in
  e.array.(output) <- result;
  exec_result_of_next_pos (e.position + 4)

let instruction_equals : instruction = { opcode = 8; param_count = 3 }

let execution_equals (e : execution) : execution_result =
  let params_a = Array.of_list e.params_and_mode in
  let m1, i1 = params_a.(0) in
  let m2, i2 = params_a.(1) in
  let _, io = params_a.(2) in
  let input1 = if m1 == 0 then e.array.(i1) else i1 in
  let input2 = if m2 == 0 then e.array.(i2) else i2 in
  let output = io in
  let result = if input1 == input2 then 1 else 0 in
  e.array.(output) <- result;
  exec_result_of_next_pos (e.position + 4)

let instruction_halt : instruction = { opcode = 99; param_count = 0 }

let instruction_of_opcode (opcode : int) : instruction =
  match opcode with
  | 0 -> instruction_halt
  | 1 -> instruction_add
  | 2 -> instruction_mul
  | 3 -> instruction_input
  | 4 -> instruction_print
  | 5 -> instruction_jmp_if_true
  | 6 -> instruction_jmp_if_false
  | 7 -> instruction_less_than
  | 8 -> instruction_equals
  | 99 -> instruction_halt
  | _ -> failwith ("Unknown instruction code: " ^ string_of_int opcode)

(* due to recursive parsing, param 0 will have index 0, and this is meant to be read left-to-right *)
let read_params (arr : int array) (from : int) (to_i : int) : int list =
  (* Printf.printf "Reading params %d-%d\n" from (to_i - 1); *)
  let rec read (i : int) (read_p : int list) : int list =
    match i with
    | _ when i == to_i -> read_p
    | _ -> read (i + 1) (arr.(i) :: read_p)
  in
  List.rev (read from [])

let _read_execution_simple (arr : int array) (position : int) : execution =
  let opcode = arr.(position) in
  let instruction = instruction_of_opcode opcode in
  let params_start_pos = position + 1 in
  let params =
    read_params arr params_start_pos (params_start_pos + instruction.param_count)
  in
  let params_and_mode = List.map (fun p -> (0, p)) params in
  { array = arr; position; instruction; params_and_mode }

let _read_execution_with_modes (arr : int array) (position : int) : execution =
  let operation = arr.(position) in
  let operation_str : string = Basics.pad (string_of_int operation) 2 '0' in
  let opcode_from = String.length operation_str - 2 in
  let opcode_str = String.sub operation_str opcode_from 2 in
  (* Printf.printf "Opcode: %s\n" opcode_str; *)
  let instruction = instruction_of_opcode (int_of_string opcode_str) in
  let param_modes_str_unpadded =
    String.sub operation_str 0 (String.length operation_str - 2)
  in
  let param_modes_str =
    Basics.pad param_modes_str_unpadded instruction.param_count '0'
  in
  (* Printf.printf "Params: %s\n" param_modes_str; *)
  let params_start_pos = position + 1 in
  let params =
    read_params arr params_start_pos (params_start_pos + instruction.param_count)
  in
  let params_modes =
    List.rev
      (List.map
         (fun c -> int_of_string (Char.escaped c))
         (List.of_seq (String.to_seq param_modes_str)))
  in
  let params_and_mode = List.combine params_modes params in
  { array = arr; position; instruction; params_and_mode }

let read_execution (arr : int array) (position : int) : execution =
  (* Printf.printf "Reading execution starting @ %d\n" position; *)
  let opcode = arr.(position) in
  match opcode with
  | 99 ->
      {
        array = arr;
        position;
        instruction = instruction_halt;
        params_and_mode = [];
      }
  | _ when opcode < 100 -> _read_execution_simple arr position
  | _ -> _read_execution_with_modes arr position

(* entrypoints *)

let apply_execution (e : execution) (inputs : int list) :
    execution_result * int list =
  Printf.printf "Executing opcode %d @ %d with %d and params: [ "
    e.instruction.opcode e.position e.instruction.opcode;
  List.iter (fun (i1, i2) -> Printf.printf " %d|%d " i1 i2) e.params_and_mode;
  Printf.printf " ]\n";
  let new_inputs : int list =
    if e.instruction.opcode == instruction_input.opcode then
      (* Printf.printf "Consuming input %d\n" (List.hd inputs); *)
      List.tl inputs
    else inputs
  in
  let execution_fun : execution -> execution_result =
    match e.instruction.opcode with
    | 0 -> failwith "Cannot execute a halt!!!"
    | 1 -> execution_add
    | 2 -> execution_mul
    | 3 -> execution_input (List.hd inputs)
    | 4 -> execution_print
    | 5 -> execution_jmp_if_true
    | 6 -> execution_jmp_if_false
    | 7 -> execution_less_than
    | 8 -> execution_equals
    | _ -> failwith ("Unknown opcode! " ^ string_of_int e.instruction.opcode)
  in
  (execution_fun e, new_inputs)

type intcode_program = { memory : int array; inputs : int list }

let intcode_program_of (memory : string) (inputs : string) : intcode_program =
  let memory_array = Basics.split_as_ints memory in
  let inputs_list =
    if String.length inputs > 0 then
      List.map int_of_string (String.split_on_char ',' inputs)
    else []
  in
  { memory = memory_array; inputs = inputs_list }

type intcode_program_result = { memory : int array; outputs : int list }

let execute (program : intcode_program) : intcode_program_result =
  print_string "\n\n-- Start --\n";
  print_string "| Input array: ";
  Basics.print_int_array (Array.of_list program.inputs) 0;
  Printf.printf "-----------------------\n";
  let rec execute_from (position : int) (inputs : int list) (outputs : int list)
      : int list * int array =
    let execution = read_execution program.memory position in
    match execution.instruction with
    | instruction when instruction == instruction_halt ->
        (* Printf.printf "Got HALT @ %d\n-- Done --\n\n" position; *)
        (* print_string "Result: "; *)
        (* Basics.print_int_array program.memory 0; *)
        (outputs, program.memory)
    | __ ->
        let exec_result, remaining_inputs = apply_execution execution inputs in
        let new_outputs =
          match exec_result.output with
          | None -> outputs
          | Some new_out -> new_out :: outputs
        in
        execute_from exec_result.next_position remaining_inputs new_outputs
  in
  let outputs, memory = execute_from 0 program.inputs [] in
  Printf.printf "-- Finished --\n";
  Printf.printf "Outputs: ";
  Basics.print_int_array (Array.of_list outputs) 0;
  Printf.printf "--------------\n\n";
  { memory; outputs }

let () = print_endline "Loaded IntCode virtual machine"
