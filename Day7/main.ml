open Commons
open Commons.Basics
open Commons.IntcodeComputer

let input : string list = IOUtils.read_all_lines "Day7/input"

let run_phase (starting_memory : int array) (inputs : int list) :
    intcode_program_result =
  IntcodeComputer.execute { memory = starting_memory; inputs }

let do_amplification (program : string) (phases : int list) :
    intcode_program_result =
  let rec amplify (phases : int list) (previous : intcode_program_result) =
    match phases with
    | [] -> previous
    | phase :: remaining_phases ->
        let phase_result =
          run_phase (split_as_ints program) (phase :: previous.outputs)
        in
        amplify remaining_phases phase_result
  in
  amplify phases { memory = split_as_ints program; outputs = [ 0 ] }

let run_sample (program : string) (phases : int list) : unit =
  let result = do_amplification program phases in
  let name = String.concat "" (List.map string_of_int phases) in
  Printf.printf "\n- AMP for %s => %d\n" name (List.hd result.outputs)

let samples () =
  run_sample "3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0" [ 4; 3; 2; 1; 0 ];
  run_sample
    "3,23,3,24,1002,24,10,24,1002,23,-1,23,101,5,23,23,1,24,23,23,4,23,99,0,0"
    [ 0; 1; 2; 3; 4 ];
  run_sample
    "3,31,3,32,1002,32,10,32,1001,31,-2,31,1007,31,0,33,1002,33,7,33,1,33,31,31,1,32,31,31,4,31,99,0,0,0"
    [ 1; 0; 4; 3; 2 ];
  print_endline "Samples over"

type amplification = string -> int list -> intcode_program_result

let find_best_phase (input : string) (permutations : int list list)
    (tarace : amplification) =
  let phases_results =
    List.map (fun phases -> (phases, tarace input phases)) permutations
  in
  let by_amp_level =
    List.sort
      (fun (_, r1) (_, r2) ->
        Int.compare (List.hd r1.outputs) (List.hd r2.outputs))
      phases_results
  in
  let phase, result = List.hd (List.rev by_amp_level) in
  Printf.printf "Found best amplification signal to be %d with permutation: "
    (List.hd result.outputs);
  print_int_array (Array.of_list phase) 0;
  print_endline "Done"

let part1 () =
  let input = List.hd (IOUtils.read_all_lines "Day7/input") in
  let phases_permutations = Basics.permutations [ 0; 1; 2; 3; 4 ] in
  find_best_phase input phases_permutations do_amplification

let () = samples ()
