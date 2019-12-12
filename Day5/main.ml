open Commons

let split_as_ints(input: string): (int array) =
  let input_split = String.split_on_char ',' input in
  let input_ints = List.map int_of_string input_split in
  Array.of_list input_ints

let exec_with(program: string) (inputs: string): (int array) =
  let result = IntcodeComputer.execute (IntcodeComputer.intcode_program_of program inputs) in
  result.memory

let samples() = 
  (* let _ = exec_with "3,0,4,0,99" in *)
  (* let _ = exec_with "1002,4,3,4,33" in
  let _ = exec_with "1101,100,-1,4,0" in *)
  (* let _ = exec_with "3,9,8,9,10,9,4,9,99,-1,8" in *)
  (* let _ = exec_with "3,9,7,9,10,9,4,9,99,-1,8" in *)
  (* let _ = exec_with "3,3,1108,-1,8,3,4,3,99" in *)
  (* let _ = exec_with "3,3,1107,-1,8,3,4,3,99" in *)
  (* let _ = exec_with "3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9" in *)
  (* let _ = exec_with "3,3,1105,-1,9,1101,0,0,12,4,12,99,1" in *)
  (* let _ = exec_with "3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99" in *)
  print_endline "(Samples over!)"

let part1() =
  (* input 1 *)
  let input = IOUtils.read_all_lines "Day5/input"
  in exec_with (List.hd input) "1"

let part2() =
  (* input 5 *)
  let input = IOUtils.read_all_lines "Day5/input"
  in exec_with (List.hd input) "5"

let () =
  (* let _ = part1() in *)
  (* let _ = samples() in *)
  let _ = part2() in
  print_endline "Done"
