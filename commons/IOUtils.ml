let read_all_lines(path: string): (string list) =
  let channel = open_in path in
  let rec read_lines (channel: in_channel) (lines_so_far: (string list)): (string list) =
    try
      let line = input_line channel in read_lines channel (line::lines_so_far)
    with _ -> lines_so_far
  in let res = read_lines channel [] in
  close_in_noerr channel; 
  res
