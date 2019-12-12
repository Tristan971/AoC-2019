open Commons
open Commons.Basics

let input: string list = IOUtils.read_all_lines "Day7/input"

let exec_program(program: string) (inputs: int list): int =
  let memory_init = split_as_ints program in
  let intcode_program: IntcodeComputer.intcode_program = {
    memory = memory_init;
    inputs = inputs
  } in

  let result = IntcodeComputer.execute intcode_program in
  let output = List.hd result.outputs in
  (* Printf.printf "Finished with output: %d and array: " output;
  Basics.print_int_array result.memory 0; *)
  output

let do_amplification (program: string) (phases: int list): int =
  let rec amplify (phases: int list) (previous: int) =
    match phases with
    | [] -> previous
    | phase::remaining_phases -> let result = exec_program program [phase; previous] in amplify remaining_phases result
  in amplify phases 0

let run_sample (program: string) (phases: int list): unit =
  let result = do_amplification program phases in
  let name = String.concat "" (List.map string_of_int phases) in
  Printf.printf "\n- AMP for %s => %d\n" name result

let samples() =
  run_sample "3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0" [4; 3; 2; 1; 0];
  print_endline "Samples over"

let () =
  let _ = samples() in
  print_endline "Done"
