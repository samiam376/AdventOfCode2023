open Core

let data = In_channel.read_lines "data/16.txt"
let data_len = List.length data
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

let beam_bfs ~queue ~seen ~shadow =
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
        shadow.(fst cords).(snd cords) <- '#';
        let nq direction = Queue.enqueue queue (next_node cords direction) in
        match value with
        | '.' ->
          nq dir;
          visit ()
        | '|' ->
          (match dir with
           | Up | Down ->
             nq dir;
             visit ()
           | Left | Right ->
             nq Up;
             nq Down;
             visit ())
        | '-' ->
          (match dir with
           | Left | Right ->
             nq dir;
             visit ()
           | Up | Down ->
             nq Left;
             nq Right;
             visit ())
        | '\\' ->
          (match dir with
           | Up ->
             nq Left;
             visit ()
           | Down ->
             nq Right;
             visit ()
           | Left ->
             nq Up;
             visit ()
           | Right ->
             nq Down;
             visit ())
        | '/' ->
          (match dir with
           | Up ->
             nq Right;
             visit ()
           | Down ->
             nq Left;
             visit ()
           | Left ->
             nq Down;
             visit ()
           | Right ->
             nq Up;
             visit ())
        | c -> failwith ("unexpected char: " ^ String.of_char c))
  in
  visit ()
;;

let calc_engery start_node =
  let shadow_grid = Array.make_matrix ~dimx:rows ~dimy:cols '.' in
  let node_set = NodeSet.create () in
  let node_q = Queue.create () in
  Queue.enqueue node_q start_node;
  beam_bfs ~queue:node_q ~seen:node_set ~shadow:shadow_grid;
  let shadow_sum =
    shadow_grid
    |> Array.to_sequence
    |> Sequence.map ~f:Array.to_sequence
    |> Sequence.concat
    |> Sequence.fold ~init:0 ~f:(fun acc curr ->
      if Char.equal '#' curr then acc + 1 else acc)
  in
  shadow_sum
;;

let print_grid arr =
  Array.iter arr ~f:(fun row ->
    Array.iter row ~f:(fun col -> printf "%c" col);
    printf "\n")
;;

(*pt 1 *)
let s = calc_engery { cords = 0, 0; dir = Right }

(*p2 find max energy start*)
let top = Sequence.range 0 cols |> Sequence.map ~f:(fun v -> { cords = 0, v; dir = Down })

let bottom =
  Sequence.range 0 cols |> Sequence.map ~f:(fun v -> { cords = rows - 1, v; dir = Up })
;;

let left =
  Sequence.range 0 rows |> Sequence.map ~f:(fun v -> { cords = v, 0; dir = Right })
;;

let right =
  Sequence.range 0 rows |> Sequence.map ~f:(fun v -> { cords = v, cols - 1; dir = Left })
;;

let max =
  [ top; bottom; left; right ]
  |> Sequence.of_list
  |> Sequence.concat
  |> Sequence.map ~f:(fun n -> calc_engery n)
  |> Sequence.max_elt ~compare
;;

match max with
| Some m -> printf "max energy: %d\n" m
| None -> printf "no max found"
