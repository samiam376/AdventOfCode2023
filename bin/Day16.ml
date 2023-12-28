open Core

let data = In_channel.read_lines "data/16.txt"
let grid = data |> List.map ~f:String.to_array |> Array.of_list
let rows = Array.length grid
let cols = Array.length grid.(0)

type direction =
  | Up
  | Down
  | Left
  | Right
[@@deriving sexp]

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

let rec beam (row, col) dir seen ~shadow =
  let key = cord_key (row, col) dir in
  if inbounds (row, col) && not (Set.mem seen key)
  then (
    let value = grid.(row).(col) in
    let next = next_cords (row, col) in
    let updated = Set.add seen key in
    shadow.(row).(col) <- '#';
    match value with
    | '.' -> beam ~shadow (next dir) dir updated
    | '|' ->
      (match dir with
       | Up | Down -> beam ~shadow (next dir) dir updated
       | Left | Right ->
         beam ~shadow (next Up) Up updated;
         beam ~shadow (next Down) Down updated)
    | '-' ->
      (match dir with
       | Left | Right -> beam ~shadow (next dir) dir updated
       | Up | Down ->
         beam ~shadow (next Right) Right updated;
         beam ~shadow (next Left) Left updated)
    | '\\' ->
      (match dir with
       | Up -> beam ~shadow (next Left) Left updated
       | Down -> beam ~shadow (next Right) Right updated
       | Left -> beam ~shadow (next Up) Up updated
       | Right -> beam ~shadow (next Down) Down updated)
    | '/' ->
      (match dir with
       | Up -> beam ~shadow (next Right) Right updated
       | Down -> beam ~shadow (next Left) Left updated
       | Left -> beam ~shadow (next Down) Down updated
       | Right -> beam ~shadow (next Up) Up updated)
    | c -> failwith ("unexpected char: " ^ String.of_char c))
  else ()
;;

let shadow_grid = Array.make_matrix ~dimx:rows ~dimy:cols '.';;

beam (0, 0) Right String.Set.empty ~shadow:shadow_grid

let print_grid arr =
  Array.iter arr ~f:(fun row ->
    Array.iter row ~f:(fun col -> printf "%c" col);
    printf "\n")
;;

let sum =
  shadow_grid
  |> Array.to_sequence
  |> Sequence.map ~f:Array.to_sequence
  |> Sequence.concat
  |> Sequence.fold ~init:0 ~f:(fun acc curr ->
    if Char.equal '#' curr then acc + 1 else acc)
;;

printf "sum: %d\n" sum
