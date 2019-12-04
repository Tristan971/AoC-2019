open Commons

type path = { direction : string; length : int }

let string_of_path (path : path) : string =
  "{ direction: " ^ path.direction ^ ", length: " ^ string_of_int path.length
  ^ " }"

type pos = { x : int; y : int }

let string_of_pos (pos : pos) : string =
  "{ x: " ^ string_of_int pos.x ^ ", y: " ^ string_of_int pos.y ^ " }"

let get_delta (direction : string) : pos =
  match direction with
  | "U" -> { x = 0; y = -1 }
  | "D" -> { x = 0; y = 1 }
  | "R" -> { x = 1; y = 0 }
  | "L" -> { x = -1; y = 0 }
  | _ -> failwith ("Unknown direction: " ^ direction)

let apply_delta (position : pos) (delta : pos) : pos =
  { x = position.x + delta.x; y = position.y + delta.y }

let get_visited_positions_for_path (start : pos) (path : path) : pos list =
  let rec travel_path (start : pos) (delta : pos) (remaining : int)
      (positions : pos list) =
    match remaining with
    | 0 -> positions
    | _ ->
        let new_pos = apply_delta start delta in
        travel_path new_pos delta (remaining - 1) (new_pos :: positions)
  in
  let delta = get_delta path.direction in
  travel_path start delta path.length []

let get_visited_positions (start : pos) (paths : path list) : pos list =
  let rec travel (start : pos) (paths : path list) (visited : pos list) :
      pos list =
    match paths with
    | [] -> visited
    | path :: remaining ->
        let newly_visited : pos list =
          get_visited_positions_for_path start path
        in
        let last_pos : pos = List.hd newly_visited in
        travel last_pos remaining (newly_visited @ visited)
  in
  travel start paths []

let get_manhattan_distance (position : pos) : int =
  let deltaX = abs position.x in
  let deltaY = abs position.y in
  deltaX + deltaY

module SS = Set.Make (String)

let get_intersections (positions1 : pos list) (positions2 : pos list): (pos list) =
  let pos_to_str(pos: pos) = (string_of_int pos.x) ^ "|" ^ (string_of_int pos.y) in
  let pos_from_str(str: string) = let split = String.split_on_char '|' str in { x=(int_of_string (List.nth split 0)); y=(int_of_string (List.nth split 1))} in
  let inter_1 = SS.of_list (List.map pos_to_str positions1) in
  let inter_2 = SS.of_list (List.map pos_to_str positions2) in
  let inter = SS.inter inter_1 inter_2 in
  print_endline ("Intersections set cardinal: " ^ (string_of_int (SS.cardinal inter)));
  let inter_list = SS.elements inter in
  List.map pos_from_str inter_list

let get_closest_intersection (intersections : pos list) : pos =
  let compare_by_manhattan_distance (pos1 : pos) (pos2 : pos) =
    Int.compare (get_manhattan_distance pos1) (get_manhattan_distance pos2)
  in
  let sorted = List.sort compare_by_manhattan_distance intersections in
  List.hd sorted

let parse_path (path_str : string) : path =
  let direction = String.sub path_str 0 1 in
  let length =
    int_of_string (String.sub path_str 1 (String.length path_str - 1))
  in
  { direction; length }

let find_closest_intersection (wire1 : path list) (wire2 : path list) : pos =
  let center : pos = { x = 0; y = 0 } in
  let pos1 = get_visited_positions center wire1 in
  print_endline
    ("Wire 1 had: " ^ string_of_int (List.length pos1) ^ " positions.");
  let pos2 = get_visited_positions center wire2 in
  print_endline
    ("Wire 2 had: " ^ string_of_int (List.length pos2) ^ " positions.");

  let intersections = get_intersections pos1 pos2 in
  print_endline
    ("Intersections count: " ^ string_of_int (List.length intersections));
  get_closest_intersection intersections

let parse_input (wires : string list) : path list list =
  let parse_wire (wire : string) : path list =
    List.map parse_path (String.split_on_char ',' wire)
  in
  let wire1 = parse_wire (List.nth wires 0) in
  let wire2 = parse_wire (List.nth wires 1) in
  [ wire1; wire2 ]

let run_for (input : string list) : unit =
  let wires = parse_input input in
  let closest_inters =
    find_closest_intersection (List.nth wires 0) (List.nth wires 1)
  in
  let dist_of_closes = get_manhattan_distance closest_inters in
  print_endline
    ( "Closest intersection is "
    ^ string_of_pos closest_inters
    ^ " with distance "
    ^ string_of_int dist_of_closes
    ^ ".\n" )

let samples () : unit =
  run_for [ "R8,U5,L5,D3"; "U7,R6,D4,L4" ];
  run_for
    [ "R75,D30,R83,U83,L12,D49,R71,U7,L72"; "U62,R66,U55,R34,D71,R55,D58,R83" ];
  run_for
    [
      "R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51";
      "U98,R91,D20,R16,D67,R40,U7,R15,U6,R7";
    ]

let part1 () : unit =
  let input = IOUtils.read_all_lines "./Day3/input" in
  run_for input

let () = part1()
