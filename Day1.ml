
open Commons

let fuel_of_module_mass(mass: int): int = 
  let db3 = mass / 3 in
  let rdw = int_of_float (floor (float_of_int db3))in
  rdw - 2

let main: unit = 
  let modules: (string list) = IOUtils.read_all_lines "Day1_input" in
  let modules_masses: (int list) = List.map int_of_string modules in
  let rec print_mass(elements: int list): unit = 
    match elements with
      | h::t -> print_int h; print_mass t
      | [] -> print_string "Done!"
  in print_mass modules_masses
