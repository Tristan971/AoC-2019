open Commons
open Commons.Basics

type noun_verb = {
  noun: int;
  verb: int;
}

let split_as_ints(input: string): (int array) =
  let input_split = String.split_on_char ',' input in
  let input_ints = List.map int_of_string input_split in
  Array.of_list input_ints

let exec_for(input: string) =
  let result = IntcodeComputer.execute (split_as_ints input) in
  Array.to_list result

let samples(): unit = 
  (*let _ = exec_for("1,0,0,0,99") in
  let _ = exec_for("2,3,0,3,99") in *)
  (*let _ = exec_for("2,4,4,5,99,0") in*)
  let _ = exec_for("1,1,1,4,99,5,6,0,99") in
  print_string "Done running samples\n"


let exec_with(input: string)(params: noun_verb): (int array) =
  let input_arr: (int array) = split_as_ints input in
  Array.set input_arr 1 params.noun;
  Array.set input_arr 2 params.verb;
  Commons.IntcodeComputer.execute input_arr

let read_input(): string = 
  let input = IOUtils.read_all_lines "./Day2/input" in
  match input with
    | h::_ -> h
    | _ -> failwith "Invalid input given!"

let part1(): unit =
  print_string "\n-- PART 1 --\n";
  
  let input = read_input() in
  
  let params: noun_verb = {
    noun=12; 
    verb=2
  } in 
  print_int_array (exec_with input params) 0

let part2(): unit =
  print_string "\n-- PART 2 --\n";
  
  let input = read_input() in
  
  let next(params: noun_verb): noun_verb = 
    let next_params =
      if params.noun < 99 then { noun = (params.noun + 1); verb = params.verb }
      else if params.verb < 99 then { noun = 0; verb = (params.verb + 1) }
      else  failwith("No next after noun at " ^ string_of_int params.noun ^ " and verb at " ^ string_of_int params.verb)
    in (*Printf.printf "-> Next params: { %d, %d }\n" next_params.noun next_params.verb;*) next_params
  in

  let rec search_em(params: noun_verb) =
    let result = exec_with input params in
    let output = Array.get result 0 in
    match output with
      | 19690720 -> params
      | _ -> search_em (next params)
  in
  let solution = search_em({noun=0; verb=0}) in
  Printf.printf "Solution was: { noun=%d; verb=%d }\n" solution.noun solution.verb;
  Printf.printf "Yielding secret: %d\n" (solution.noun * 100 + solution.verb)


let () =
  part1();
  part2();
  (*samples();*)
  print_string "Over and out.\n\n"
