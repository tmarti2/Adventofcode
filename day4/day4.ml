let between min max s_value =
  let value = int_of_string s_value in
  value >= min && value <= max

let is_field_valid_one field =
  try
    Scanf.sscanf field "%3s:%s" (fun key value ->
        match key with
        | "byr" | "iyr" | "eyr" | "hgt" | "hcl" | "ecl" | "pid" -> 1
        | _ -> 0
      )
  with
  | End_of_file | Failure _ | Scanf.Scan_failure _ -> 0

let is_field_valid_two field =
  try
    let res =
      Scanf.sscanf field "%3s:%s" (fun key value ->
          match key with
          | "byr" -> between 1920 2002 value
          | "iyr" -> between 2010 2020 value
          | "eyr" -> between 2020 2030 value
          | "hgt" ->
            let size,u = Scanf.sscanf value "%d%s" (fun s u -> s,u) in
            (u = "cm" && size >= 150 && size <= 193)
            || (u = "in" && size >= 59 && size <= 76)
          | "hcl" ->
            let str_color =
              Str.regexp "^#[a-fA-F0-9][a-fA-F0-9][a-fA-F0-9][a-fA-F0-9][a-fA-F0-9][a-fA-F0-9]$"
            in
            Str.string_match str_color value 0
          | "ecl" ->
            value = "amb" || value = "blu" || value = "brn" ||
            value = "gry" || value = "grn" || value = "hzl" || value = "oth"
          | "pid" ->
            let str_numb =
              Str.regexp "^[0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9]$"
            in
            Str.string_match str_numb value 0
          | _ -> false
        ) in
    if res then 1 else 0
  with
  | End_of_file | Failure _ | Scanf.Scan_failure _ -> 0

let parse_line f line =
  let split_line = String.split_on_char ' ' line in
  List.fold_left (fun acc field ->
      (f field) + acc
    ) 0 split_line

let fold f input =
  let rec fold_aux valid_fields valids =
    try
      let line = input_line input in
      if line = "" then
        if valid_fields = 7 then
          fold_aux 0 (valids+1)
        else
          fold_aux 0 valids
      else
        fold_aux (valid_fields+parse_line f line) valids
    with End_of_file -> if valid_fields = 7 then valids+1 else valids
  in
  fold_aux 0 0

let read_data f filepath =
  let input = open_in filepath in
  let valids = fold f input in
  close_in input;
  valids

let () =
  let valids = read_data is_field_valid_one "data" in
  Printf.printf "Part one : %d\n" valids;
  let valids = read_data is_field_valid_two "data" in
  Printf.printf "Part two : %d\n" valids