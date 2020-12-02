let separators = Str.regexp "[- :]+"
exception Too_many

let read_data f filepath =
  let input = open_in filepath in
  let good, wrong = f input 0 0 in
  close_in input;
  good, wrong

let rec part_one input good wrong =
  try
    let line = input_line input in
    let str_list = Str.split separators line in
    match str_list with
    | min::max::letter::pwd::[] ->
      let min = int_of_string min in
      let max = int_of_string max in
      let cpt = ref 0 in
      String.iter (fun c ->
          let str_c = Char.escaped c in
          if String.equal letter str_c then begin
            incr cpt;
            if !cpt > max then raise Too_many
          end
        ) pwd;
      if !cpt >= min then
        part_one input (good+1) wrong
      else
        part_one input good (wrong+1)
    | _ ->
      Printf.printf "Wrong line format: %s\n" line;
      part_one input good (wrong+1)
  with
  | End_of_file -> good, wrong
  | Too_many -> part_one input good (wrong+1)


let rec part_two input good wrong =
  try
    let line = input_line input in
    let str_list = Str.split separators line in
    match str_list with
    | min::max::letter::pwd::[] ->
      let fst = int_of_string min in
      let lst = int_of_string max in
      let len = String.length pwd in
      if len < lst then part_two input good (wrong+1)
      else begin
        let fst_c = Char.escaped pwd.[fst-1] in
        let lst_c = Char.escaped pwd.[lst-1] in
        if (fst_c = letter) <> (lst_c = letter) then
          part_two input (good+1) wrong
        else
          part_two input good (wrong+1)
      end
    | _ ->
      Printf.printf "Wrong line format\n";
      part_two input good (wrong+1)
  with
   End_of_file -> good, wrong

let () =
  let good, wrong = read_data part_one "data"in
  Printf.printf "Part one :\n";
  Printf.printf "%d wrong passwords\n" wrong;
  Printf.printf "%d good passwords\n" good;
  let good, wrong = read_data part_two "data" in
  Printf.printf "Part two :\n";
  Printf.printf "%d wrong passwords\n" wrong;
  Printf.printf "%d good passwords\n" good;