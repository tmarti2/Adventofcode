exception Found of int

let to_binary str =
  let bin = String.map (fun c ->
      match c with
      | 'F' | 'L' -> '0'
      | 'B' | 'R' -> '1'
      | _ -> assert false) str
  in
  int_of_string @@ "0b"^bin

let get_row_id row =
  Scanf.sscanf row "%7s%3s" ( fun row col ->
      let row = to_binary row in
      let col = to_binary col in
      row * 8 + col
    )

let rec parse input acc max=
  try
    let line = input_line input in
    let id = get_row_id line in
    if id > max then parse input (id::acc) id else parse input (id::acc) max
  with
  | End_of_file -> acc,max

let compute input =
  let all_ids, max = parse input [] (-1) in
  let sorted = List.sort compare all_ids in
  match sorted with
  | [] | [_] -> assert false
  | hd :: tl ->
    let previous = ref hd in
    try
      List.iter (fun el ->
          if el <> !previous+1 then
            raise (Found (el-1))
          else
            previous:=el
        ) tl;
      assert false;
    with Found id -> id, max

let read_data f filepath =
  let input = open_in filepath in
  let max_id,my_id = f input in
  close_in input;
  max_id,my_id

let () =
  let max_id,my_id = read_data compute "data" in
  Printf.printf "Part one : %d\n" max_id;
  Printf.printf "Part two : %d\n" my_id