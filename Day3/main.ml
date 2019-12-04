type path = { direction : string; length : int }

type pos = { x : int; y : int }

let get_delta (direction : string) : pos =
  match direction with
  | "U" -> { x = 0; y = -1 }
  | _ -> failwith ("Unknown direction!" ^ direction)

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
