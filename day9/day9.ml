exception Found of int*int

let rec k_upto acc k n l =
  if List.length l < k then raise Not_found;
  match l with
  | [] -> raise Not_found
  | [hd] ->
    if hd = n then hd::acc else raise Not_found
  | hd :: tl ->
    try
      if k = 1 && hd = n then hd::acc
      else if k > 1 && hd < n then
        k_upto (hd::acc) (k-1) (n-hd) tl
      else raise Not_found
    with Not_found -> k_upto acc k n tl

let iter_from size data =
  let prv = Queue.create () in
  try
    Array.iteri (fun i e ->
        if Queue.length prv < size then Queue.add e prv
        else begin
          let list = List.rev @@ Queue.fold (fun acc e-> e::acc ) [] prv in
          try
            let _l = k_upto [] 2 e list in
            ignore(Queue.pop prv); Queue.add e prv;
          with Not_found -> raise (Found (i,e))
        end
      ) data;
    None
  with Found (i,e) -> Some (i,e)

let parse_data input =
  let rec parse_aux acc =
    try
      let line = input_line input in
      parse_aux ((int_of_string line)::acc)
    with
    | End_of_file -> acc
    | Failure _ -> parse_aux acc
  in
  parse_aux []

let read_data filepath =
  let input = open_in filepath in
  let res = List.rev (parse_data input) in
  close_in input;
  Array.of_list res

let () =
  let data = read_data "data" in
  let find_wrong = iter_from 25 data in
  match find_wrong with
  | None -> Printf.printf "Everything is ok...\n"
  | Some (i,e) ->
    Printf.printf "Part one : %d\n" e;
    let min,max = ref 0, ref 0 in
    try
      for i' = 0 to i-2 do
        let count = ref (data.(i')) in
        min:=!count;max:=!count;
        try
          for j = i'+1 to i-1 do
            let v = data.(j) in
            count:=!count+v;
            if v > !max then max:=v;
            if v < !min then min:=v;
            if !count = e then raise (Found (i',j))
            else if !count > e then raise Not_found
          done
        with Not_found -> ()
      done
    with Found _ ->
      Printf.printf "Part two : %d\n" (!max + !min);
