let read_data f filepath =
  let input = open_in filepath in
  let good, wrong = f input 0 0 in
  close_in input;
  good, wrong

let rec part_one input good wrong =
  try
    let line = input_line input in
    Scanf.sscanf line "%d-%d %c: %s" (fun min max c s ->
        let cpt = ref 0 in
        String.iter (fun c' -> if c = c' then incr cpt) s;
        if !cpt < min || !cpt > max then
          part_one input good (wrong+1)
        else
          part_one input (good+1) wrong
      )
  with
  | End_of_file -> good, wrong
  | Scanf.Scan_failure _ | Failure _ ->
    Printf.printf "Wrong line format\n";
    part_one input good (wrong+1)

let rec part_two input good wrong =
  try
    let line = input_line input in
    Scanf.sscanf line "%d-%d %c: %s" (fun fst lst c s ->
        let len = String.length s in
        if len < lst then part_two input good (wrong+1)
        else begin
          if (s.[fst-1] = c) <> (s.[lst-1] = c) then
            part_two input (good+1) wrong
          else
            part_two input good (wrong+1)
        end
      )
  with
  | End_of_file -> good, wrong
  | Scanf.Scan_failure _ | Failure _ ->
    Printf.printf "Wrong line format\n";
    part_two input good (wrong+1)

let () =
  let good, wrong = read_data part_one "data"in
  Printf.printf "Part one :\n";
  Printf.printf "%d wrong passwords\n" wrong;
  Printf.printf "%d good passwords\n" good;
  let good, wrong = read_data part_two "data" in
  Printf.printf "Part two :\n";
  Printf.printf "%d wrong passwords\n" wrong;
  Printf.printf "%d good passwords\n" good;