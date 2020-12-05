exception Found of int

let explode s = List.init (String.length s) (String.get s)

let mid lb ub = lb + (ub - lb) / 2

let get_new_bound (lb,ub) c =
  match c with
  | 'F' | 'L' -> (lb, mid lb ub)
  | 'B' | 'R' -> (mid lb (ub+1),ub)
  | _ -> assert false

let get_row_id row =
  Scanf.sscanf row "%7s%3s" ( fun row col ->
      let f = fun acc c -> get_new_bound acc c in
      let lbr,ubr = List.fold_left f (0,127) (explode row) in
      let lbc,ubc = List.fold_left f (0,7) (explode col) in
      if lbr!=ubr || lbc!=ubc then assert false;
      (lbr*8+lbc)
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