open Core

type matrix_iterator =
  { matrix : char array array
  ; current_idx : int * int
  ; dims : int * int
  }

let create_char_matrix file =
  let lines = file |> List.map ~f:(fun x -> String.to_array x) in
  List.to_array lines
;;

let new_cord (row, col) (dr, dc) = row + dr, col + dc
let possible_adj_cordinates = [ 0, 1; 0, -1; 1, 0; 1, 1; 1, -1; -1, 0; -1, 1; -1, -1 ]
let top (r, _c) = r <> -1
let bottom (r, _c) = r <> 1
let left (_r, c) = c <> -1
let right (_r, c) = c <> 1

let get_adj_cord iter =
  let row, col = iter.current_idx in
  let rows, cols = iter.dims in
  let max_col_idx = cols - 1 in
  let max_row_idx = rows - 1 in
  let filters =
    match row, col with
    (* first corner *)
    | 0, 0 -> fun x -> top x && left x
    (* second corner *)
    | 0, col when col = max_col_idx -> fun x -> top x && right x
    (* third corner *)
    | row, 0 when row = max_row_idx -> fun x -> bottom x && left x
    (* fourth corner *)
    | row, y when row = max_row_idx && y = max_col_idx -> fun x -> bottom x && right x
    (* top row *)
    | row, _y when row = 0 -> fun x -> top x
    (* left edge *)
    | _row, _col when col = 0 -> fun x -> left x
    (* right edge *)
    | _row, _col when col = max_col_idx -> fun x -> right x
    (* bottom row *)
    | row, _col when row = max_row_idx -> fun x -> bottom x
    | _ -> fun _ -> true
  in
  let movements = List.filter ~f:filters possible_adj_cordinates in
  let coords = List.map ~f:(new_cord (row, col)) movements in
  coords
;;

let print_option_cords = Option.iter ~f:(fun x -> printf "%d, %d" (fst x) (snd x))

let get_next_cord iter =
  let row, col = iter.current_idx in
  let nrows, ncols = iter.dims in
  let next =
    match row, col with
    | r, c when r = nrows - 1 && c = ncols - 1 -> None
    | r, c when c = ncols - 1 -> Some (r + 1, 0)
    | r, c -> Some (r, c + 1)
  in
  next
;;

let value_or_fail iter cords =
  try iter.matrix.(fst cords).(snd cords) with
  | _ ->
    "Failed to get value at "
    ^ string_of_int (fst cords)
    ^ ", "
    ^ string_of_int (snd cords)
    |> failwith
;;

let get_value iter = value_or_fail iter iter.current_idx

let get_next_value iter =
  let next = get_next_cord iter in
  Option.map next ~f:(fun (r, c) -> value_or_fail iter (r, c), (r, c))
;;

let capture_adj_values iter =
  let adj_cords = get_adj_cord iter in
  List.fold
    ~init:[]
    ~f:(fun acc (r, c) ->
      let v = value_or_fail iter (r, c) in
      v :: acc)
    adj_cords
;;

let capture_adj_values_with_cords iter =
  let adj_cords = get_adj_cord iter in
  List.fold
    ~init:[]
    ~f:(fun acc (r, c) ->
      let v = value_or_fail iter (r, c) in
      (v, (r, c)) :: acc)
    adj_cords
;;

let is_adj_to_special_char iter =
  let adj_values = capture_adj_values iter in
  List.exists adj_values ~f:(fun x -> (not (Char.is_digit x)) && not (Char.equal x '.'))
;;

let adj_asterix iter =
  let adj_values = capture_adj_values_with_cords iter in
  let ast = List.find adj_values ~f:(fun (x, _) -> Char.equal x '*') in
  Option.map ast ~f:(fun (_, cords) -> cords)
;;

let capture_number iter =
  let rec aux iter acc =
    let value = get_value iter in
    let digit =
      match value with
      | '0' .. '9' ->
        let adj = is_adj_to_special_char iter in
        Some (value, adj)
      | _ -> None
    in
    let next = get_next_value iter in
    match next, digit with
    | None, Some d -> List.rev (d :: acc), iter
    | Some (v, cords), Some d when Char.is_digit v ->
      aux { iter with current_idx = cords } (d :: acc)
    | Some (v, _), Some d when not (Char.is_digit v) -> List.rev (d :: acc), iter
    | _ -> List.rev acc, iter
  in
  let captured, iter = aux iter [] in
  let is_adj = List.exists captured ~f:(fun (_, x) -> x) in
  let char_list = List.map captured ~f:(fun (x, _) -> x) in
  let s = String.of_char_list char_list in
  let number = Int.of_string s in
  if is_adj then Some number, iter else None, iter
;;

module Cord = struct
  type t = int * int [@@deriving sexp, compare]
end

module CordMap = Map.Make (Cord)
module CordSet = Set.Make (Cord)

let capture_number2 iter cord_map =
  let rec aux iter acc =
    let value = get_value iter in
    let digit =
      match value with
      | '0' .. '9' ->
        let adj = adj_asterix iter in
        Some (value, adj)
      | _ -> None
    in
    let next = get_next_value iter in
    match next, digit with
    | None, Some d -> List.rev (d :: acc), iter
    | Some (v, cords), Some d when Char.is_digit v ->
      aux { iter with current_idx = cords } (d :: acc)
    | Some (v, _), Some d when not (Char.is_digit v) -> List.rev (d :: acc), iter
    | _ -> List.rev acc, iter
  in
  let captured, iter = aux iter [] in
  let adj_asterixes = List.filter_map captured ~f:(fun (_, x) -> x) in
  let deduped = CordSet.stable_dedup_list adj_asterixes in
  let char_list = List.map captured ~f:(fun (x, _) -> x) in
  let s = String.of_char_list char_list in
  let number = Int.of_string s in
  let map =
    List.fold deduped ~init:cord_map ~f:(fun acc x ->
      CordMap.update acc x ~f:(function
        | None -> [ number ]
        | Some l -> number :: l))
  in
  map, iter
;;

let traverse iter =
  let rec aux iter acc =
    let value = get_value iter in
    match value with
    | '0' .. '9' ->
      let num, iter2 = capture_number iter in
      let next_cord = get_next_cord iter2 in
      (match next_cord with
       | Some c -> aux { iter2 with current_idx = c } (num :: acc)
       | None -> List.rev (num :: acc))
    | _ ->
      (match get_next_cord iter with
       | Some cords -> aux { iter with current_idx = cords } acc
       | None -> List.rev acc)
  in
  let nums = aux iter [] in
  let nums = List.filter_opt nums in
  let sum = List.fold ~init:0 ~f:( + ) nums in
  printf "sum: %d\n" sum
;;

let traverse2 iter cord_map =
  let rec aux iter cord_map =
    let value = get_value iter in
    match value with
    | '0' .. '9' ->
      let cord_map, iter2 = capture_number2 iter cord_map in
      let next_cord = get_next_cord iter2 in
      (match next_cord with
       | Some c -> aux { iter2 with current_idx = c } cord_map
       | None -> cord_map)
    | _ ->
      (match get_next_cord iter with
       | Some cords -> aux { iter with current_idx = cords } cord_map
       | None -> cord_map)
  in
  let map = aux iter cord_map in
  let sum =
    CordMap.fold map ~init:0 ~f:(fun ~key:_ ~data acc ->
      if List.length data = 2
      then (
        let first = List.hd_exn data in
        let second = List.nth_exn data 1 in
        acc + (first * second))
      else acc)
  in
  printf "sum: %d\n" sum
;;

let file = In_channel.read_lines "data/3a.txt" in
let matrix = create_char_matrix file in
let rows = Array.length matrix in
let cols = Array.length matrix.(0) in
let iter = { matrix; current_idx = 0, 0; dims = rows, cols } in
let map = CordMap.empty in
traverse2 iter map
