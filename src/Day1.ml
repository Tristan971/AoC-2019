
let double (x: int): int = x * x

let main: unit = 
  let value = 2 in
  let print_result (num: int): unit = Printf.printf "%d * %d is: %d" num num (double num) in
  print_result value

