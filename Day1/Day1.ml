open Commons

let () = 
  let list_of_modules = IOUtils.read_all_lines "./Day1/input" in
  print_string "Read "; print_int (List.length list_of_modules); print_string " modules"
