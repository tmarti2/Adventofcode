module QS = Set.Make(struct type t = char let compare = compare end)

let explode s = List.init (String.length s) (String.get s)

let rec add_q_to_set str set =
  List.fold_left (fun set c -> QS.add c set) set (explode str)

let rec count_yes ?(first=false) ?(every=false) input acc set =
  try
    let line = input_line input in
    if line = "" then
      count_yes input ~first:true ~every ((QS.cardinal set)+acc) QS.empty
    else begin
      let new_set =
        if not first && every then
          add_q_to_set line QS.empty |> QS.inter set
        else
          add_q_to_set line set
      in
      count_yes ~every input acc new_set
    end
  with
  | End_of_file -> (QS.cardinal set)+acc

let read_data f filepath =
  let input = open_in filepath in
  let count = f input 0 QS.empty in
  close_in input;
  count

let () =
  let tt = read_data (count_yes ~first:true) "data" in
  Printf.printf "Part one : %d\n" tt;
  let tt = read_data (count_yes ~first:true ~every:true) "data" in
  Printf.printf "Part two : %d\n" tt