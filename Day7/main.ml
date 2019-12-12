open Commons
open Commons.Basics
open Commons.IntcodeComputer

let input : string list = IOUtils.read_all_lines "Day7/input"

let exec_program (program : string) (inputs : int list) : int =
  let memory = split_as_ints program in
  let intcode_program : intcode_program = { memory; inputs } in
  let result = IntcodeComputer.execute intcode_program in
  List.hd result.outputs

let do_amplification (program : string) (phases : int list) : int =
  let rec amplify (phases : int list) (previous : int) =
    match phases with
    | [] -> previous
    | phase :: remaining_phases ->
        let result = exec_program program [ phase; previous ] in
        amplify remaining_phases result
  in
  amplify phases 0

(* let run_sample (program : string) (phases : int list) : unit =
  let result = do_amplification program phases in
  let name = String.concat "" (List.map string_of_int phases) in
  Printf.printf "\n- AMP for %s => %d\n" name result

let samples () =
  run_sample "3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0" [ 4; 3; 2; 1; 0 ];
  run_sample
    "3,23,3,24,1002,24,10,24,1002,23,-1,23,101,5,23,23,1,24,23,23,4,23,99,0,0"
    [ 0; 1; 2; 3; 4 ];
  run_sample
    "3,31,3,32,1002,32,10,32,1001,31,-2,31,1007,31,0,33,1002,33,7,33,1,33,31,31,1,32,31,31,4,31,99,0,0,0"
    [ 1; 0; 4; 3; 2 ];
  print_endline "Samples over" *)

let part1 () =
  let input = List.hd (IOUtils.read_all_lines "Day7/input") in
  let phases_permutations = Basics.permutations [ 0; 1; 2; 3; 4 ] in
  let phases_and_signal =
    List.map
      (fun phases -> (phases, do_amplification input phases))
      phases_permutations
  in
  let by_amp_level =
    List.sort (fun (_, s1) (_, s2) -> Int.compare s1 s2) phases_and_signal
  in
  let phases, signal = List.hd (List.rev by_amp_level) in
  Printf.printf "Found best amplification signal to be %d with permutation: "
    signal;
  print_int_array (Array.of_list phases) 0;
  print_endline "Done"

let part2 () =
  let input = List.hd (IOUtils.read_all_lines "Day7/input") in
  let phases_permutations = Basics.permutations [ 0; 1; 2; 3; 4 ] in
  let phases_and_signal =
    List.map
      (fun phases -> (phases, do_amplification input phases))
      phases_permutations
  in
  let by_amp_level =
    List.sort (fun (_, s1) (_, s2) -> Int.compare s1 s2) phases_and_signal
  in
  let phases, signal = List.hd (List.rev by_amp_level) in
  Printf.printf "Found best amplification signal to be %d with permutation: "
    signal;
  print_int_array (Array.of_list phases) 0;
  print_endline "Done"

let () = part1 ()
