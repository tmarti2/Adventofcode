type slope = {right:int;down:int}

let count_trees slope input =
  let rec fold i offset trees =
    try
      let line = input_line input in
      if i > 0 && i mod slope.down = 0 then begin
        let next_offset = (offset+slope.right) mod String.length line in
        if line.[next_offset] = '#' then
          fold (i+1) next_offset (trees+1)
        else
          fold (i+1) next_offset trees
      end
      else
        fold (i+1) offset trees
    with
    | End_of_file -> trees
  in
  fold 0 0 0

let read_data slope filepath =
  let input = open_in filepath in
  let trees = count_trees slope input in
  close_in input;
  trees

let () =
  let p1 = read_data {right=3;down=1} "data" in
  Printf.printf "Slope : (Right:%d, Down:%d) = %d trees\n" 3 1 p1;

  let slopes = [{right=1;down=1};{right=5;down=1};{right=7;down=1};{right=1;down=2}] in
  let p2 = List.fold_left (fun acc slope ->
      let trees = read_data slope "data" in
      Printf.printf "Slope : (Right:%d, Down:%d) = %d trees\n" slope.right slope.down trees;
      acc*trees
    ) 1 slopes
  in
  Printf.printf "Part one : %d\n" p1;
  Printf.printf "Part two : %d\n" (p1*p2)