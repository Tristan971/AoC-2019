open Commons

let split_as_ints(input: string): (int array) =
  let input_split = String.split_on_char ',' input in
  let input_ints = List.map int_of_string input_split in
  Array.of_list input_ints

let exec_with(input: string): (int array) =
  let input_arr: (int array) = split_as_ints input in
  IntcodeComputer.execute input_arr

let samples() = 
  (* let _ = exec_with "3,0,4,0,99" in *)
  let _ = exec_with "1002,4,3,4,33" in
  let _ = exec_with "1101,100,-1,4,0" in
  print_endline "(Samples over!)"

let () =
  let _ = samples() in
  print_endline "Done"
