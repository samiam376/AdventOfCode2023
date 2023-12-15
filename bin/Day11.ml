open Core

module Cord = struct
  type t = int * int [@@deriving sexp, compare]
end

module CordSet = Set.Make (Cord)

let is_galaxy c = Char.equal c '#'
let not_galaxy c = not (is_galaxy c)
let input = In_channel.read_lines "data/11.txt"
let chars = List.map input ~f:String.to_array |> Array.of_list
let rows = Array.length chars
let cols = Array.length chars.(0)
let col_idxs = List.range 0 cols
let row_idxs = List.range 0 rows

let rows_to_expand =
  List.fold ~init:[] row_idxs ~f:(fun acc r ->
    let row = chars.(r) in
    if Array.for_all row ~f:not_galaxy then r :: acc else acc)
;;

let cols_to_expand =
  List.fold ~init:[] col_idxs ~f:(fun acc c ->
    let col = Array.map chars ~f:(fun row -> row.(c)) in
    if Array.for_all col ~f:not_galaxy then c :: acc else acc)
;;

let sort_tupule (x1, x2) = if x1 < x2 then x1, x2 else x2, x1
let distance (x1, y1) (x2, y2) = abs (x2 - x1) + abs (y2 - y1)

let expanded_distance (x1, y1) (x2, y2) ~rows_to_expand ~cols_to_expand ~factor =
  printf "distance (%d, %d) (%d, %d)\n" x1 y1 x2 y2;
  let x1, x2 = sort_tupule (x1, x2) in
  let y1, y2 = sort_tupule (y1, y2) in
  let x_expansion =
    List.filter rows_to_expand ~f:(fun r -> r > x1 && r < x2) |> List.length
  in
  let y_expansion =
    List.filter cols_to_expand ~f:(fun c -> c > y1 && c < y2) |> List.length
  in
  printf "x_expansion: %d\n" x_expansion;
  printf "y_expansion: %d\n" y_expansion;
  let d =
    distance (x1, y1) (x2, y2)
    + (x_expansion * (factor - 1))
    + (y_expansion * (factor - 1))
  in
  printf "distance: %d\n" d;
  d
;;

printf "num rows to expand: %d\n" (List.length rows_to_expand);
printf "num cols to expand: %d\n" (List.length cols_to_expand);
let new_cords = List.cartesian_product (List.range 0 rows) (List.range 0 cols) in
let galaxy_cords = List.filter new_cords ~f:(fun (r, c) -> is_galaxy chars.(r).(c)) in
let all_pairs = List.cartesian_product galaxy_cords galaxy_cords in
let shortest_path =
  List.map all_pairs ~f:(fun (c1, c2) ->
    expanded_distance c1 c2 ~rows_to_expand ~cols_to_expand ~factor:1_000_000)
in
let total = List.fold shortest_path ~init:0 ~f:( + ) in
let final = total / 2 in
printf "%d\n" final
