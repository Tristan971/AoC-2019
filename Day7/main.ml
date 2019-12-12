open Commons

let input: string list = IOUtils.read_all_lines "Day7/input"

let exec_program(program: string) (inputs: string): int =
  let result = IntcodeComputer.execute (IntcodeComputer.intcode_program_of program inputs) in
  let output = List.hd result.outputs in
  Printf.printf "Finished with output: %d and array:\n" output;
  Basics.print_int_array result.memory 0;
  output

let samples() =
  exec_program "3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0" "4,3,2,1,0"

let () =
  let _ = samples() in
  print_endline "Done"
