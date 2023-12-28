open Core

let data = In_channel.read_lines "data/14.txt"
let grid = List.map data ~f:(fun s -> String.to_array s) |> Array.of_list

let print_array arr =
  Array.iter arr ~f:(fun row ->
    Array.iter row ~f:(fun col -> printf "%c" col);
    printf "\n")
;;

let deep_copy_array arr =
  let copy = Array.map ~f:Array.copy arr in
  copy
;;

let rows = Array.length grid
let cols = Array.length grid.(0)

(* printf "Initial Grid \n"; *)
(* print_array grid *)

let can_roll above = Char.equal above '.'
let is_o value = Char.equal value 'O'

(*pt 1: roll north*)
(* let () = *)
(*   for r = 1 to rows - 1 do *)
(*     for c = 0 to cols - 1 do *)
(*       for row_above = r - 1 downto 0 do *)
(*         let current_value = grid.(row_above + 1).(c) in *)
(*         let value_above = grid.(row_above).(c) in *)
(*         (* printf "current value: %c, value above: %c\n" current_value value_above; *) *)
(*         if is_o current_value && can_roll value_above *)
(*         then ( *)
(*           (* printf "rolling\n"; *) *)
(*           grid.(row_above).(c) <- current_value; *)
(*           grid.(row_above + 1).(c) <- value_above) *)
(*         else () *)
(*       done *)
(*     done *)
(*   done *)
(* ;; *)

let roll_north grid =
  for r = 1 to rows - 1 do
    for c = 0 to cols - 1 do
      for row_above = r - 1 downto 0 do
        let current_value = grid.(row_above + 1).(c) in
        let value_above = grid.(row_above).(c) in
        (* printf "current value: %c, value above: %c\n" current_value value_above; *)
        if is_o current_value && can_roll value_above
        then (
          (* printf "rolling\n"; *)
          grid.(row_above).(c) <- current_value;
          grid.(row_above + 1).(c) <- value_above)
        else ()
      done
    done
  done
;;

let roll_west grid =
  for r = 0 to rows - 1 do
    for c = 1 to cols - 1 do
      for col_left = c - 1 downto 0 do
        let current_value = grid.(r).(col_left + 1) in
        let value_left = grid.(r).(col_left) in
        (* printf "current value: %c, value above: %c\n" current_value value_above; *)
        if is_o current_value && can_roll value_left
        then (
          (* printf "rolling\n"; *)
          grid.(r).(col_left) <- current_value;
          grid.(r).(col_left + 1) <- value_left)
        else ()
      done
    done
  done
;;

let roll_south grid =
  for r = rows - 2 downto 0 do
    for c = 0 to cols - 1 do
      for row_below = r + 1 to rows - 1 do
        let current_value = grid.(row_below - 1).(c) in
        let value_below = grid.(row_below).(c) in
        (* printf "current value: %c, value above: %c\n" current_value value_above; *)
        if is_o current_value && can_roll value_below
        then (
          (* printf "rolling\n"; *)
          grid.(row_below).(c) <- current_value;
          grid.(row_below - 1).(c) <- value_below)
        else ()
      done
    done
  done
;;

let roll_east grid =
  for r = 0 to rows - 1 do
    for c = cols - 2 downto 0 do
      for col_right = c + 1 to cols - 1 do
        let current_value = grid.(r).(col_right - 1) in
        let value_right = grid.(r).(col_right) in
        (* printf "current value: %c, value above: %c\n" current_value value_above; *)
        if is_o current_value && can_roll value_right
        then (
          (* printf "rolling\n"; *)
          grid.(r).(col_right) <- current_value;
          grid.(r).(col_right - 1) <- value_right)
        else ()
      done
    done
  done
;;

let roll grid =
  let copied_grid = deep_copy_array grid in
  roll_north copied_grid;
  roll_west copied_grid;
  roll_south copied_grid;
  roll_east copied_grid;
  copied_grid
;;

let sum grid =
  Array.foldi grid ~init:0 ~f:(fun row_idx acc row ->
    acc
    + Array.fold row ~init:0 ~f:(fun acc curr ->
      if is_o curr then acc + (rows - row_idx) else acc))
;;

let grid_string grid =
  grid
  |> Array.to_sequence
  |> Sequence.map ~f:Array.to_sequence
  |> Sequence.concat
  |> Sequence.to_list
  |> String.of_char_list
;;

module StrMap = Map.Make (String)

let cycle grid =
  let rec aux grid n cache =
    let gs = grid_string grid in
    match Map.find cache gs with
    | Some prev -> prev, n - prev
    | None ->
      let next_grid = roll grid in
      let cache = Map.add_exn cache ~key:gs ~data:n in
      aux next_grid (n + 1) cache
  in
  aux grid 0 StrMap.empty
;;

let n_iter = 1_000_000_000
let offset, cycle_len = cycle grid;;

let num = offset + ((n_iter - offset) mod cycle_len) in
let sum =
  List.range 0 num |> List.fold ~init:grid ~f:(fun acc _curr -> roll acc) |> sum
in
printf "offest: %d, cycle_len %d \n" offset cycle_len;
printf "sum: %d\n" sum
