open Commons.Tree
open Commons.Basics

let sample_tree(): v_tree =
  Tree ( "COM", [
    Tree ("B", [
      Tree ("G", [ 
        Leaf "H" 
      ]);
      Tree ("C", [
        Tree ("D", [
          Leaf "I";
          Tree ("E", [
            Leaf "F";
            Tree ("J", [
              Tree ("K", [ 
                Leaf "L" 
              ])
            ])
          ])
        ])
      ])
    ])
  ])

type orbit = {
  center: string;
  orbiter: string;
}

type key_to_tree_list = (v_tree list StringMap.t)

let string_to_orbit(in_s: string): orbit =
  let parts = String.split_on_char ')' in_s in
  { center=List.hd parts; orbiter=List.nth parts 1 }

let parse_and_merge_trees(orbits: orbit list) =
  let _ = group_by (fun orbit -> orbit.center) orbits in
  print_endline "Incomplete"

let () = print_endline "Done"
