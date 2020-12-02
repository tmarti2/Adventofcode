let rec read_line input f good =
  try
    let line = input_line input in
    Scanf.sscanf line "%d-%d %c: %s" (fun lb ub c s ->
        read_line input f (f lb ub c s good)
      )
  with
  | End_of_file -> good
  | Scanf.Scan_failure _ | Failure _ ->
    Printf.printf "Wrong line format\n";
    read_line input f good

let read_data f filepath =
  let input = open_in filepath in
  let good = read_line input f 0 in
  close_in input;
  good

let part_one min max c s good =
  let cpt = ref 0 in
  String.iter (fun c' -> if c = c' then incr cpt) s;
  if !cpt >= min && !cpt <= max then good+1
  else good

let part_two fst lst c s good =
  if String.length s < lst || (s.[fst-1] = c) = (s.[lst-1] = c) then good
  else good+1

let () =
  let good = read_data part_one "data" in
  Printf.printf "Part one : %d good passwords\n" good;
  let good = read_data part_two "data" in
  Printf.printf "Part two : %d good passwords\n" good