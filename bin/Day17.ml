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

let create_adj_matrix () =
  let dim = rows * cols in
  Array.make_matrix ~dimy:dim ~dimx:dim 0
;;

type direction =
  | Up
  | Down
  | Left
  | Right
[@@deriving hash, sexp, equal, compare]

type cord =
  { cords : int * int
  ; dir : direction
  }
[@@deriving hash, equal, of_sexp, sexp, compare]

module CordSet = Hash_set.Make (struct
    type t = cord

    let equal = equal_cord
    let hash = hash_cord
    let t_of_sexp = cord_of_sexp
    let sexp_of_t = sexp_of_cord
    let compare = compare_cord
  end)

let inbounds (row, col) = row >= 0 && row < rows && col >= 0 && col < cols

let next_direction ~count ~dir =
  if count = 3
  then (
    match dir with
    | Up | Down -> [ Left; Right ]
    | Left | Right -> [ Up; Down ])
  else (
    match dir with
    | Up -> [ Up; Left; Right ]
    | Down -> [ Down; Left; Right ]
    | Left -> [ Left; Up; Down ]
    | Right -> [ Right; Up; Down ])
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
  let row, col = cords in
  grid.(row).(col)
;;

let find_adj () =
  let adj_matrix = create_adj_matrix () in
  let seen = CordSet.create () in
  let rec visit cords ~straight_count =
    if (not (inbounds cords.cords)) || Hash_set.mem seen cords
    then ()
    else if last_node cords.cords
    then ()
    else (
      Hash_set.add seen cords;
      let starting_vertex = to_1d cords.cords in
      let directions = next_direction ~count:straight_count ~dir:cords.dir in
      List.iter directions ~f:(fun next_dir ->
        let next = next_cords cords.cords next_dir in
        if inbounds next
        then (
          let v = get_value next in
          let ending_vertex = to_1d next in
          adj_matrix.(starting_vertex).(ending_vertex) <- v;
          let next_count =
            if equal_direction cords.dir next_dir then straight_count + 1 else 1
          in
          visit { cords = next; dir = next_dir } ~straight_count:next_count)
        else ()))
  in
  visit { cords = 0, 0; dir = Right } ~straight_count:1;
  visit { cords = 0, 0; dir = Down } ~straight_count:1;
  adj_matrix
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

let adj = find_adj ();;

print_adj adj
