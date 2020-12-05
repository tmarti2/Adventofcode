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

let find_max_id input =
  let rec fold max =
    try
      let line = input_line input in
      let id = get_row_id line in
      if id > max then fold id else fold max
    with
    | End_of_file -> max
  in
  fold (-1)

let find_my_id input =
  let rec fold acc =
    try
      let line = input_line input in
      fold (get_row_id line :: acc)
    with
    | End_of_file -> acc
  in
  let all_ids = fold [] in
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
    with Found id -> id

let read_data f filepath =
  let input = open_in filepath in
  let res = f input in
  close_in input;
  res

let () =
  let max_id = read_data find_max_id "data" in
  Printf.printf "Part one : %d\n" max_id;
  let my_id = read_data find_my_id "data" in
  Printf.printf "Part two : %d\n" my_id