open Commons.Tree
open Commons

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

module AdjacenciesMap = Map.Make(
  struct
    type t = string

    let compare = String.compare
  end
)

let read_adjacencies(adjacencies: string list) =
  let rec read_adjacencies(left: string list) (map: 'a AdjacenciesMap.t) =
    match left with
    | [] -> map
    | adjacency::t -> 
      let parts = String.split_on_char ')' adjacency in
      let parent = List.hd parts in
      let child = List.nth parts 1 in
      match AdjacenciesMap.find_opt parent map with
      | Some previously -> read_adjacencies t (AdjacenciesMap.add parent (child::previously) map)
      | None -> read_adjacencies t (AdjacenciesMap.add parent [child] map)
  in read_adjacencies adjacencies AdjacenciesMap.empty

let print_adjacencies_map(map: string list AdjacenciesMap.t): unit =
  let print_adj(k: string) (v: string list): unit =
    print_string (k ^ " -> ");
    List.iter (fun adj -> Printf.printf "%s " adj) v;
    print_newline()
  in AdjacenciesMap.iter print_adj map

let find_root(adjacencies: string list AdjacenciesMap.t): string =
  let is_root (name: string): bool =
    let parents = AdjacenciesMap.filter (fun _ children -> not (List.mem name children)) adjacencies
    in AdjacenciesMap.cardinal parents == 0
  in let (name, _) = AdjacenciesMap.find_first (fun k -> is_root k) adjacencies in
  name

let () =
  let sample_tree = sample_tree() in
  print_tree sample_tree;

  let sample_input = IOUtils.read_all_lines "Day6/input_sample" in
  let sample_adjacencies = read_adjacencies sample_input in
  print_adjacencies_map sample_adjacencies;
  Printf.printf "Root is %s" (find_root sample_adjacencies);
  print_endline "Done"
