type v_tree =
  | Leaf of string
  | Tree of string * (v_tree list)

let print_tree(tree: v_tree): unit =
  let rec printy(tree: v_tree) (depth: int): unit =
    Basics.do_x_times (depth) (fun _ -> Printf.printf "  ");
    match tree with
    | Leaf (key) -> Printf.printf "%s ]\n" key
    | Tree (key, trees) -> Printf.printf "%s ->\n" key; List.iter (fun t -> printy t (depth + 1)) trees
  in printy tree 0
