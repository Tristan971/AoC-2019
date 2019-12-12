open Commons
open Commons.Basics

let input: string list = IOUtils.read_all_lines "Day7/input"

let exec_with(input: string) (_: string list) =
  IntcodeComputer.execute (split_as_ints input)

let samples() =
  exec_with ("3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0") []

let () =
  let _ = samples() in
  print_endline "Done"
