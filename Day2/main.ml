let execute_in_place_array_op(in1: int)(in2: int)(res: int)(op: (int -> int -> int))(arr: int array): (int array) = 
  Array.set arr res (op in1 in2);
  arr

let add(in1: int)(in2: int)(res: int)(code: int array): (int array) = 
  execute_in_place_array_op in1 in2 res (fun a b -> a + b) code

let mul(in1: int)(in2: int)(res: int)(code: int array): (int array) = 
  execute_in_place_array_op in1 in2 res (fun a b -> a * b) code

type operation_params =
{
  in1: int;
  in2: int;
  res: int;
}

let rec exec(position: int) (code: (int array)): (int array) =
  let opcode = Array.get code position in

  let exec_operation(opcode: int) (position: int) (code: (int array)): (int array) =
    let in1 = Array.get code (position + 1) in
    let in2 = Array.get code (position + 2) in
    let res = Array.get code (position + 3) in
    match opcode with
        | 1 -> exec (position + 4) (add in1 in2 res code)
        | 2 -> exec (position + 4) (mul in1 in2 res code)
        | _ -> failwith("Wtf, got opcode: " ^ string_of_int opcode)
  in
  
  match opcode with
    | 99 -> code
    | 1 | 2 -> exec_operation opcode position code
    | __ -> failwith ("Invalid code: " ^ (string_of_int opcode))

let () =
  let result = exec 0 (Array.of_list [1;0;0;0;99]) in
  Printf.printf("[");
  List.iter (fun elem -> Printf.printf "%s, " (string_of_int elem)) (Array.to_list result);
  Printf.printf("]");
