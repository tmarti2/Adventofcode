open Graph

type offset = (int->int->int)*int
type inst = Nop of offset | Acc of offset | Jmp of offset

module Int = struct
  type t = int
  let compare = compare
  let hash = Hashtbl.hash
  let equal = (=)
  let default = 0
end
module G = Imperative.Digraph.ConcreteBidirectionalLabeled(Int)(Int)
module Poids = struct
  type edge = G.E.t
  type t = int
  let weight = G.E.label
  let zero = 0
  let add = (+)
  let sub = (-)
  let compare = compare
end
module Pcc = Path.Dijkstra(G)(Poids)
module Dfs = Traverse.Dfs(G)

let g = G.create ()

let fill_graph prog =
  Array.iteri (fun i inst ->
      match inst with
      | Acc _ | Nop _ ->
        G.add_edge_e g (G.E.create i 0 (i+1))
      | Jmp(f,value) ->
        G.add_edge_e g (G.E.create i 0 (f i value))
    ) prog

let alter_graph prog =
  Array.iteri (fun i inst ->
      match inst with
      | Acc _ -> ()
      | Nop(f,value) ->
        G.add_edge_e g (G.E.create i 1 (f i value))
      | Jmp _ ->
        G.add_edge_e g (G.E.create i 1 (i+1))
    ) prog

let incr_acc inst acc =
  match inst with
  | Acc(f,value) -> f acc value
  | _ -> acc

let exec_prog prog =
  Dfs.fold_component (fun inst acc ->
      incr_acc prog.(inst) acc
    ) 0 g 0

let exec_path prog path =
  let acc = incr_acc prog.(0) 0 in
  List.fold_left (fun acc (_,_,dst) ->
      incr_acc prog.(dst) acc
    ) acc path

let convert_to_inst inst signe value =
  let s = match signe with | "-" -> (-) | "+" -> (+) | _ -> assert false in
  match inst with
  | "nop" -> Nop(s,value)
  | "acc" -> Acc(s,value)
  | "jmp" -> Jmp(s,value)
  | _ -> assert false

let parse_data input =
  let rec parse_aux acc =
    try
      let line = input_line input in
      let inst = Scanf.sscanf line "%3s %1s%d" (fun inst signe value ->
          convert_to_inst inst signe value
        ) in
      parse_aux (inst::acc)
    with
    | End_of_file | Failure _ | Scanf.Scan_failure _ -> acc
  in
  parse_aux []

let read_data filepath =
  let input = open_in filepath in
  let data_l = parse_data input in
  let prog = Array.of_list (List.rev data_l) in
  close_in input;
  prog

let () =
  let prog = read_data "data" in
  fill_graph prog;
  Printf.printf "Part one : %d\n" (exec_prog prog);
  alter_graph prog;
  let el,_ = Pcc.shortest_path g 0 ((Array.length prog)-1) in
  let acc = exec_path prog el in
  Printf.printf "Part two : %d\n" acc;