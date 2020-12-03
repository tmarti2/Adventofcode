type slope = {right:int;down:int}

let rec get_next_line input down =
  let line = input_line input in
  if down > 1 then get_next_line input (down-1) else line

let rec read_line input f slope (offset,trees) =
  try
    let line = get_next_line input slope.down in
    read_line input f slope (f slope.right line (offset,trees))
  with
  | End_of_file -> trees

let read_data f slope filepath =
  let input = open_in filepath in
  ignore(get_next_line input (slope.down-1));
  let trees = read_line input f slope (0,0) in
  close_in input;
  trees

let count_trees right line (offset,trees) =
  let next_offset = (offset+right) mod String.length line in
  if line.[next_offset] = '#' then (next_offset,trees+1) else (next_offset,trees)

let () =
  let p1 = read_data count_trees {right=3;down=1} "data" in
  Printf.printf "Slope : (Right:%d, Down:%d) = %d trees\n" 3 1 p1;

  let slopes = [{right=1;down=1};{right=5;down=1};{right=7;down=1};{right=1;down=2}] in
  let p2 = List.fold_left (fun acc slope ->
      let trees = read_data count_trees slope "data" in
      Printf.printf "Slope : (Right:%d, Down:%d) = %d trees\n" slope.right slope.down trees;
      acc*trees
    ) 1 slopes
  in
  Printf.printf "Part one : %d\n" p1;
  Printf.printf "Part two : %d\n" (p1*p2)