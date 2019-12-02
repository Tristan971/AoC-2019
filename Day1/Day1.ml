open Commons

let mass_to_fuel(mass: int): int =
  int_of_float (floor ( (float_of_int mass) /. 3.0 ) ) - 2

let () = 
  let list_of_modules = IOUtils.read_all_lines "./Day1/input" in
  let list_of_module_masses = List.map int_of_string list_of_modules in
  let list_of_fuel_reqs = List.map mass_to_fuel list_of_module_masses in
  let rec sum(l: (int list)) =
    match l with
      | [] -> 0
      | h::t -> h + sum t
  in print_int (sum list_of_fuel_reqs)
