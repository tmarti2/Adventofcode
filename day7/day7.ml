open Graph

(* let color_to_id = Hashtbl.create 17 *)
(* let id_to_color = Hashtbl.create 17 *)

module Int = struct
  type t = int
  let compare = compare
  let hash = Hashtbl.hash
  let equal = (=)
  let default = 0
end
module String = struct
  type t = string
  let compare = String.compare
  let hash = Hashtbl.hash
  let equal = (=)
  let default = ""
end
module G = Imperative.Digraph.ConcreteBidirectionalLabeled(String)(Int)
module Oper = Oper.I(G)
module Dfs = Traverse.Dfs(G)

let g = G.create ()

let next =
  let id = ref 0 in
  fun () ->
    let id' = !id in incr id; id'

(* let get_color_id color =
   if Hashtbl.mem color_to_id color then
    Hashtbl.find color_to_id color
   else begin
    let id = next () in
    Hashtbl.add color_to_id color id;
    Hashtbl.add id_to_color id color;
    id
   end *)

let rec extract acc ls =
  match ls with
  | nb :: fst :: snd :: tl ->
    let nb = int_of_string nb in
    extract ((nb,fst^snd)::acc) tl
  | _ -> acc

let compute line =
  let split_line = Str.regexp "\ contain\ \\|\ bag[s]?\\|[ .,]+" in
  let splitted_line = List.rev @@ Str.split split_line line in
  let ls = List.fold_left (fun acc s ->
      if s = "" then acc else s::acc
    ) [] splitted_line
  in
  if List.length splitted_line < 5 then assert false;
  match ls with
  | fst :: snd :: tl ->
    let main_vertex = G.V.create (fst^snd) in
    let colors = extract [] tl in
    List.iter (fun (nb,color) ->
        let v = G.V.create color in
        let e = G.E.create main_vertex nb v in
        G.add_edge_e g e
      ) colors
  | _ -> assert false


let rec count node =
  let succ_e = G.succ_e g node in
  List.fold_left (fun acc e ->
      acc + (G.E.label e) + (G.E.label e) * (memo (G.E.dst e))
    ) 0 succ_e
and memo =
  let h = Hashtbl.create 17 in
  fun node ->
    try Hashtbl.find h node
    with Not_found ->
      let count = count node in
      Hashtbl.add h node count;
      count

let rec parse_line f input =
  try
    let line = input_line input in
    f line; parse_line f input
  with End_of_file -> ()

let read_data f filepath =
  let input = open_in filepath in
  parse_line f input;
  close_in input

let () =
  read_data compute "data";
  let g' = Oper.mirror g in
  let p1 = Dfs.fold_component (fun _ p1 -> p1 + 1) (-1) g' "shinygold" in
  Printf.printf "Part one : %d\n" p1;
  let t0 = Unix.gettimeofday () in
  let p2 = count "shinygold" in
  let t1 = Unix.gettimeofday () in
  let p2' = memo "shinygold" in
  let t2 = Unix.gettimeofday () in
  Printf.printf "Part two : %d - %d\n" p2 p2';
  Printf.printf "Count time : %3f\n" (t1-.t0);
  Printf.printf "Memo time : %3f\n" (t2-.t1)