open Core

let data = In_channel.read_lines "data/17.txt"

let grid =
  List.map data ~f:(fun s ->
    String.to_array s |> Array.map ~f:(fun c -> c |> Char.to_string |> Int.of_string))
  |> Array.of_list
;;

let rows = Array.length grid
let cols = Array.length grid.(0)
let to_1d (row, col) = (row * cols) + col
let to_2d vertex = vertex / cols, vertex mod cols

type direction =
  | Up
  | Down
  | Left
  | Right
[@@deriving hash, sexp, equal, compare]

type cord =
  { pos : int * int
  ; dir : direction
  ; steps : int
  ; cost : int
  }
[@@deriving hash, equal, of_sexp, sexp, compare]

type cord_key =
  { pos : int * int
  ; dir : direction
  ; steps : int
  }
[@@deriving hash, equal, of_sexp, sexp, compare]

module CordTbl = Hashtbl.Make (struct
    type t = cord_key

    let equal = equal_cord_key
    let hash = hash_cord_key
    let t_of_sexp = cord_key_of_sexp
    let sexp_of_t = sexp_of_cord_key
    let compare = compare_cord_key
  end)

let inbounds (row, col) = row >= 0 && row < rows && col >= 0 && col < cols

let next_direction ~dir =
  match dir with
  | Up -> [ Up; Left; Right ]
  | Down -> [ Down; Left; Right ]
  | Left -> [ Left; Up; Down ]
  | Right -> [ Right; Up; Down ]
;;

let next_cords (row, col) dir =
  match dir with
  | Up -> row - 1, col
  | Down -> row + 1, col
  | Left -> row, col - 1
  | Right -> row, col + 1
;;

let last_node (row, col) = row = rows - 1 && col = cols - 1
let dir_string dir = string_of_sexp (sexp_of_direction dir)

let get_value cords =
  if inbounds cords
  then (
    let row, col = cords in
    Some grid.(row).(col))
  else None
;;

let print_grid arr =
  Array.iter arr ~f:(fun row ->
    Array.iter row ~f:(fun col -> printf "%c" col);
    printf "\n")
;;

let print_adj arr =
  Array.iter arr ~f:(fun row ->
    Array.iter row ~f:(fun col -> printf "%d" col);
    printf "\n")
;;

let djikstras () =
  let min_heap = Pairing_heap.create ~cmp:(fun a b -> compare a.cost b.cost) () in
  let dist = CordTbl.create () in
  Pairing_heap.add min_heap { pos = 0, 0; dir = Right; steps = 0; cost = 0 };
  Pairing_heap.add min_heap { pos = 0, 0; dir = Down; steps = 0; cost = 0 };
  Hashtbl.add_exn dist ~key:{ pos = 0, 0; dir = Right; steps = 0 } ~data:0;
  Hashtbl.add_exn dist ~key:{ pos = 0, 0; dir = Down; steps = 0 } ~data:0;
  let rec loop () =
    match Pairing_heap.pop min_heap with
    | None -> None
    | Some u ->
      if last_node u.pos
      then Some u.cost
      else if match Hashtbl.find dist { pos = u.pos; dir = u.dir; steps = u.steps } with
              | None -> false
              | Some dist -> dist < u.cost
      then loop ()
      else (
        let neighbors =
          next_direction ~dir:u.dir
          |> List.filter_map ~f:(fun d ->
            let nc = next_cords u.pos d in
            let next_value = get_value nc in
            Option.map next_value ~f:(fun cost ->
              { pos = nc
              ; dir = d
              ; cost = u.cost + cost
              ; steps = (if equal_direction d u.dir then u.steps + 1 else 1)
              }))
        in
        List.iter neighbors ~f:(fun n ->
          if n.steps > 3
             ||
             match Hashtbl.find dist { pos = n.pos; dir = n.dir; steps = n.steps } with
             | None -> false
             | Some d -> d < n.cost
          then ()
          else (
            Pairing_heap.add min_heap n;
            Hashtbl.update dist { pos = n.pos; dir = n.dir; steps = n.steps } ~f:(fun _ ->
              n.cost)));
        loop ())
  in
  loop ()
;;

let m = djikstras () |> Option.value_exn |> printf "value: %d\n"
