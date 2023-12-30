open Core

let data = In_channel.read_lines "data/16.txt"
let data_len = List.length data;;

printf "data len: %d\n" data_len

let first_string = List.hd_exn data |> String.length;;

printf "first string len: %d\n" first_string

let grid = data |> List.map ~f:String.to_array |> Array.of_list
let rows = Array.length grid
let cols = Array.length grid.(0)

type direction =
  | Up
  | Down
  | Left
  | Right
[@@deriving hash, sexp, equal, compare]

let inbounds (row, col) = row >= 0 && row < rows && col >= 0 && col < cols

let next_cords (row, col) dir =
  match dir with
  | Up -> row - 1, col
  | Down -> row + 1, col
  | Left -> row, col - 1
  | Right -> row, col + 1
;;

let cord_key (row, col) dir =
  let key =
    Int.to_string row ^ Int.to_string col ^ Sexp.to_string (sexp_of_direction dir)
  in
  key
;;

type node =
  { cords : int * int
  ; dir : direction
  }
[@@deriving hash, equal, of_sexp, sexp, compare]

module NodeSet = Hash_set.Make (struct
    type t = node

    let equal = equal_node
    let hash = hash_node
    let t_of_sexp = node_of_sexp
    let sexp_of_t = sexp_of_node
    let compare = compare_node
  end)

let next_node cords dir =
  let nc = next_cords cords dir in
  { cords = nc; dir }
;;

let beam_bfs ~queue ~seen =
  let rec visit () =
    match Queue.dequeue queue with
    | None -> ()
    | Some node ->
      let cords = node.cords in
      let dir = node.dir in
      if (not (inbounds cords)) || Hash_set.mem seen node
      then visit ()
      else (
        let value = grid.(fst cords).(snd cords) in
        Hash_set.add seen node;
        match value with
        | '.' ->
          Queue.enqueue queue (next_node cords dir);
          visit ()
        | '|' ->
          (match dir with
           | Up | Down ->
             Queue.enqueue queue (next_node cords dir);
             visit ()
           | Left | Right ->
             Queue.enqueue queue (next_node cords Up);
             Queue.enqueue queue (next_node cords Down);
             visit ())
        | '-' ->
          (match dir with
           | Left | Right ->
             Queue.enqueue queue (next_node cords dir);
             visit ()
           | Up | Down ->
             Queue.enqueue queue (next_node cords Left);
             Queue.enqueue queue (next_node cords Right);
             visit ())
        | '\\' ->
          (match dir with
           | Up ->
             Queue.enqueue queue (next_node cords Left);
             visit ()
           | Down ->
             Queue.enqueue queue (next_node cords Right);
             visit ()
           | Left ->
             Queue.enqueue queue (next_node cords Up);
             visit ()
           | Right ->
             Queue.enqueue queue (next_node cords Down);
             visit ())
        | '/' ->
          (match dir with
           | Up ->
             Queue.enqueue queue (next_node cords Right);
             visit ()
           | Down ->
             Queue.enqueue queue (next_node cords Left);
             visit ()
           | Left ->
             Queue.enqueue queue (next_node cords Down);
             visit ()
           | Right ->
             Queue.enqueue queue (next_node cords Up);
             visit ())
        | c -> failwith ("unexpected char: " ^ String.of_char c))
  in
  visit ()
;;

let rec beam (row, col) ~dir ~seen ~shadow =
  let key = cord_key (row, col) dir in
  if inbounds (row, col) && not (Hashtbl.mem seen key)
  then (
    (* printf "at row: %d, col: %d\n" row col; *)
    let value = grid.(row).(col) in
    let next = next_cords (row, col) in
    Hashtbl.add_exn seen ~key ~data:true;
    shadow.(row).(col) <- '#';
    let continue direction = beam (next direction) ~shadow ~seen ~dir:direction in
    match value with
    | '.' -> continue dir
    | '|' ->
      (match dir with
       | Up | Down -> continue dir
       | Left | Right ->
         continue Up;
         continue Down)
    | '-' ->
      (match dir with
       | Left | Right -> continue dir
       | Up | Down ->
         continue Right;
         continue Left)
    | '\\' ->
      (match dir with
       | Up -> continue Left
       | Down -> continue Right
       | Left -> continue Up
       | Right -> continue Down)
    | '/' ->
      (match dir with
       | Up -> continue Right
       | Down -> continue Left
       | Left -> continue Down
       | Right -> continue Up)
    | c -> failwith ("unexpected char: " ^ String.of_char c))
  else ()
;;

let shadow_grid = Array.make_matrix ~dimx:rows ~dimy:cols '.'
let table = Hashtbl.create (module String)

(* beam (0, 0) ~dir:Right ~seen:table ~shadow:shadow_grid *)

let node_set = NodeSet.create ()
let node_q = Queue.create ();;

Queue.enqueue node_q { cords = 0, 0; dir = Right };;
beam_bfs ~queue:node_q ~seen:node_set

let print_grid arr =
  Array.iter arr ~f:(fun row ->
    Array.iter row ~f:(fun col -> printf "%c" col);
    printf "\n")
;;

let tup_to_string (row, col) = Int.to_string row ^ Int.to_string col
let set_list = Hash_set.to_list node_set;;

printf "set len: %d\n" (List.length set_list)

let sum =
  node_set
  |> Hash_set.to_list
  |> List.map ~f:(fun n -> tup_to_string n.cords)
  |> List.dedup_and_sort ~compare:compare_string
  |> List.length
;;

(* print_grid grid *)
(* print_grid shadow_grid *)

(* let sum = *)
(*   shadow_grid *)
(*   |> Array.to_sequence *)
(*   |> Sequence.map ~f:Array.to_sequence *)
(*   |> Sequence.concat *)
(*   |> Sequence.fold ~init:0 ~f:(fun acc curr -> *)
(*     if Char.equal '#' curr then acc + 1 else acc) *)
(* ;; *)

printf "rows: %d, cols: %d\n" rows cols;
printf "sum: %d\n" sum
