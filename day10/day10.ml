open Graph

module Int = struct
  type t = int
  let compare = compare
  let hash = Hashtbl.hash
  let equal = (=)
  let default = 0
end
module G = Imperative.Digraph.ConcreteBidirectionalLabeled(Int)(Int)

let g = G.create ()

let create_graph data =
  let last = List.fold_left (fun prev curr ->
      let e = G.E.create prev (curr-prev) curr in
      G.add_edge_e g e;
      curr
    ) 0 data
  in
  let e = G.E.create last 3 (last+3) in
  G.add_edge_e g e;
  last

let add_edge src dst =
  if G.mem_vertex g dst && not (G.mem_edge g src dst) then begin
    let e = G.E.create src (dst-src) dst in
    G.add_edge_e g e
  end

let alter_graph () =
  G.iter_vertex (fun src ->
      add_edge src (src+1);
      add_edge src (src+2);
      add_edge src (src+3);
    ) g

let rec count src dst =
    if src = dst then 1
    else begin
      let succ = G.succ g src in
      List.fold_left (fun acc succ' ->
          acc + (memo succ' dst)
        ) 0 succ
    end
and memo =
  let h = Hashtbl.create 17 in
  fun src dst ->
  try Hashtbl.find h src
  with Not_found ->
    let count = count src dst in
    Hashtbl.add h src count;
    count

let count_diff (j1,j3,acc) e =
  let diff = e-acc in
  if diff = 1 then (j1+1,j3,e) else (j1,j3+1,e)

let parse_data input =
  let rec parse_aux prev acc =
    try
      let line = input_line input in
      let curr = int_of_string line in
      parse_aux curr (curr::acc)
    with
    | End_of_file -> acc
    | Failure _ -> parse_aux prev acc
  in
  parse_aux 0 []

let read_data filepath =
  let input = open_in filepath in
  let res = parse_data input in
  close_in input;
  List.rev res

let () =
  let data = read_data "data" in
  let sorted = List.sort compare data in
  let (j1,j3,_acc) = List.fold_left count_diff (0,0,0) sorted in
  let last = create_graph sorted in
  alter_graph ();
  let t0 = Unix.gettimeofday () in
  let test1 = count 0 last in
  let t1 = Unix.gettimeofday () in
  let test2 = memo 0 last in
  let t2 = Unix.gettimeofday () in
  Printf.printf "Part one : %d*%d = %d\n" j1 (j3+1) (j1*(j3+1));
  Printf.printf "Part two %d - %d\n" test1 test2;
  Printf.printf "Count time : %f\n" (t1-.t0);
  Printf.printf "Memo time : %f\n" (t2-.t1)