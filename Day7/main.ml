open Commons
open Commons.Basics
open Commons.IntcodeComputer

let input : string list = IOUtils.read_all_lines "Day7/input"

let run_phase (starting_memory : int array) (inputs : int list) :
    intcode_program_result =
  IntcodeComputer.execute { memory = starting_memory; inputs }

let do_amplification (program : string) (phases : int list) : int =
  let rec amplify (phases : int list) (previous : intcode_program_result) =
    match phases with
    | [] -> previous
    | phase :: remaining_phases ->
        let phase_result =
          run_phase (split_as_ints program) (phase :: previous.outputs)
        in
        amplify remaining_phases phase_result
  in
  let final =
    amplify phases { memory = split_as_ints program; outputs = [ 0 ] }
  in
  List.hd final.outputs

module IntTbl = Hashtbl.Make (struct
  type t = int

  let equal = Int.equal

  let hash v = v
end)

let feedback_loop_amplifications (program : string) (phases : int list) =
  let nb_phases = List.length phases in
  let phases_arr = Array.of_list phases in
  let memories = IntTbl.create nb_phases in

  Printf.printf "Permutation: ";
  Basics.print_int_array phases_arr 0;

  let rec initial (phases_left : int list) (previous_sig : int) =
    match phases_left with
    | [] -> previous_sig
    | phase :: t ->
        (* Printf.printf "Initializing phase %d\n" phase; *)
        let memory = split_as_ints program in
        let inputs = [ phase; previous_sig ] in
        (* Printf.printf "Will be using inputs: \n"; *)
        (* Basics.print_int_array (Array.of_list inputs) 0; *)
        let res = execute { memory; inputs } in
        IntTbl.add memories phase res;
        initial t (List.hd res.outputs)
  in

  let rec feedback (i : int) (previous_sig : int) : int =
    let name = phases_arr.(i) in
    let prev_res = IntTbl.find memories name in
    let memory = prev_res.memory in
    let inputs = [ previous_sig ] in
    let res = execute { memory; inputs } in
    match res.outputs with
    | [] -> previous_sig
    | h :: _ ->
        IntTbl.replace memories name res;
        feedback ((i + 1) mod nb_phases) h
  in
  try feedback 0 (initial phases 0)
  with _ -> 0

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

type amplification = string -> int list -> int

let find_best_phase (input : string) (permutations : int list list)
    (tarace : amplification) =
  let phases_results =
    List.map (fun phases -> (phases, tarace input phases)) permutations
  in
  let by_amp_level =
    List.sort (fun (_, r1) (_, r2) -> Int.compare r1 r2) phases_results
  in
  let phase, result = List.hd (List.rev by_amp_level) in
  Printf.printf "Best amplification signal is %d with permutation: " result;
  print_int_array (Array.of_list phase) 0;
  print_endline "Done"

let part1 () =
  let input = List.hd (IOUtils.read_all_lines "Day7/input") in
  let phases_permutations = Basics.permutations [ 5; 6; 7; 8; 9 ] in
  find_best_phase input phases_permutations do_amplification

let part2 () =
  let _ = List.hd (IOUtils.read_all_lines "Day7/input") in
  let phases_permutations = Basics.permutations [ 5; 6; 7; 8; 9 ] in
  let inps = "3,26,1001,26,-4,26,3,27,1002,27,2,27,1,27,26,27,4,27,1001,28,-1,28,1005,28,6,99,0,0,5" in
  find_best_phase inps phases_permutations feedback_loop_amplifications

let () = part2 ()
