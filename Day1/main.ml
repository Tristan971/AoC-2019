open Commons

let mass_to_fuel (mass : int) : int =
  int_of_float (floor (float_of_int mass /. 3.0)) - 2

let rec mass_to_fuel_2 (accounted : int) (newly_added_fuel : int) : int =
  let newly_added_fuel_fuel = mass_to_fuel newly_added_fuel in
  match newly_added_fuel_fuel <= 0 with
  | true -> accounted
  | false ->
      mass_to_fuel_2 (accounted + newly_added_fuel_fuel) newly_added_fuel_fuel

let masses : int list =
  let list_of_modules = IOUtils.read_all_lines "./Day1/input" in
  List.map int_of_string list_of_modules

let total_fuel_using (part : int) (mass_to_fuel_op : int -> int) : unit =
  let list_of_fuel_reqs = List.map mass_to_fuel_op masses in
  let total_fuel = List.fold_left (fun a b -> a + b) 0 list_of_fuel_reqs in
  Printf.printf "Part %d: %d\n" part total_fuel

let part_1 () : unit = total_fuel_using 1 mass_to_fuel

let part_2 () : unit = total_fuel_using 2 (mass_to_fuel_2 0)

let () =
  part_1 ();
  part_2 ()
