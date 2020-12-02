let rec read_line input f good wrong =
  try
    let line = input_line input in
    Scanf.sscanf line "%d-%d %c: %s" (fun lb ub c s ->
        let good, wrong = f lb ub c s good wrong in
        read_line input f good wrong
      )
  with
  | End_of_file -> good, wrong
  | Scanf.Scan_failure _ | Failure _ ->
    Printf.printf "Wrong line format\n";
    read_line input f good wrong

let read_data f filepath =
  let input = open_in filepath in
  let good, wrong = read_line input f 0 0 in
  close_in input;
  good, wrong

let part_one min max c s good wrong =
  let cpt = ref 0 in
  String.iter (fun c' -> if c = c' then incr cpt) s;
  if !cpt < min || !cpt > max then
    good, wrong+1
  else
    good+1, wrong

let part_two fst lst c s good wrong =
  let len = String.length s in
  if len < lst then good, wrong+1
  else if (s.[fst-1] = c) <> (s.[lst-1] = c) then
    good+1, wrong
  else
    good, wrong+1

let () =
  let good, wrong = read_data part_one "data" in
  Printf.printf "Part one :\n";
  Printf.printf "%d wrong passwords\n" wrong;
  Printf.printf "%d good passwords\n" good;
  let good, wrong = read_data part_two "data" in
  Printf.printf "Part two :\n";
  Printf.printf "%d wrong passwords\n" wrong;
  Printf.printf "%d good passwords\n" good;