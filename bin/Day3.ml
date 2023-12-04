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

let is_adj_to_special_char iter =
  let adj_values = capture_adj_values iter in
  List.exists adj_values ~f:(fun x -> (not (Char.is_digit x)) && not (Char.equal x '.'))
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

let search_left iter =
  let rec aux iter acc =
    if snd iter.current_idx = -1
    then acc
    else (
      match get_value iter with
      | '0' .. '9' ->
        aux
          { iter with current_idx = fst iter.current_idx, snd iter.current_idx - 1 }
          (get_value iter :: acc)
      | _ -> acc)
  in
  aux iter []
;;

let search_right iter =
  let rec aux iter acc =
    if snd iter.current_idx = snd iter.dims
    then acc
    else (
      match get_value iter with
      | '0' .. '9' ->
        aux
          { iter with current_idx = fst iter.current_idx, snd iter.current_idx + 1 }
          (get_value iter :: acc)
      | _ -> List.rev acc)
  in
  aux iter []
;;

let parse_number iter =
  let current = get_value iter in
  let left =
    search_left { iter with current_idx = fst iter.current_idx, snd iter.current_idx - 1 }
  in
  let right =
    search_right
      { iter with current_idx = fst iter.current_idx, snd iter.current_idx + 1 }
  in
  let combined = left @ (current :: right) in
  let s = String.of_char_list combined in
  let num = Int.of_string s in
  num
;;

let can_search_up iter = fst iter.current_idx <> 0
let can_search_down iter = fst iter.current_idx <> fst iter.dims - 1
let can_search_left iter = snd iter.current_idx <> 0
let can_search_right iter = snd iter.current_idx <> snd iter.dims - 1

let search_left iter =
  if can_search_left iter
  then (
    let left_iter =
      { iter with current_idx = fst iter.current_idx, snd iter.current_idx - 1 }
    in
    let value = get_value left_iter in
    if Char.is_digit value then Some (parse_number left_iter) else None)
  else None
;;

let search_right iter =
  if can_search_right iter
  then (
    let right_iter =
      { iter with current_idx = fst iter.current_idx, snd iter.current_idx + 1 }
    in
    let value = get_value right_iter in
    if Char.is_digit value then Some (parse_number right_iter) else None)
  else None
;;

let search_up iter =
  if can_search_up iter
  then (
    let above_iter =
      { iter with current_idx = fst iter.current_idx - 1, snd iter.current_idx }
    in
    let above_value = get_value above_iter in
    let above_number =
      if Char.is_digit above_value then Some (parse_number above_iter) else None
    in
    let numbers =
      match above_number with
      | Some n -> [ Some n ]
      | None -> [ search_left above_iter; search_right above_iter ]
    in
    numbers)
  else [ None ]
;;

let search_down iter =
  if can_search_down iter
  then (
    let below_iter =
      { iter with current_idx = fst iter.current_idx + 1, snd iter.current_idx }
    in
    let below_value = get_value below_iter in
    let below_number =
      if Char.is_digit below_value then Some (parse_number below_iter) else None
    in
    let numbers =
      match below_number with
      | Some n -> [ Some n ]
      | None -> [ search_left below_iter; search_right below_iter ]
    in
    numbers)
  else [ None ]
;;

(* idea find all the gears then search for numbers near by *)
let find_adj_numbers iter =
  let numbers =
    search_up iter @ search_down iter @ [ search_left iter; search_right iter ]
  in
  let nums = List.filter_opt numbers in
  print_endline "---";
  printf "current idx: (%d, %d): " (fst iter.current_idx) (snd iter.current_idx);
  List.iter nums ~f:(fun x -> printf " %d, " x);
  print_endline "";
  print_endline "---";
  nums
;;

let traverse2 iter =
  let rec aux iter acc =
    let value = get_value iter in
    match value with
    | '*' ->
      let adj_nums = find_adj_numbers iter in
      let gear_ratio =
        if List.length adj_nums = 2
        then (
          let first = List.hd_exn adj_nums in
          let second = List.nth_exn adj_nums 1 in
          Some (first * second))
        else None
      in
      let next_cord = get_next_cord iter in
      (match next_cord with
       | Some c -> aux { iter with current_idx = c } (gear_ratio :: acc)
       | None -> List.rev (gear_ratio :: acc))
    | _ ->
      (match get_next_cord iter with
       | Some cords -> aux { iter with current_idx = cords } acc
       | None -> List.rev acc)
  in
  let nums = aux iter [] in
  let nums = List.filter_opt nums in
  let sum = List.fold ~init:0 ~f:( + ) nums in
  printf "sum of gear ratios: %d\n" sum
;;

(* let file = In_channel.read_lines "data/3a.txt" in *)
(* let matrix = create_char_matrix file in *)
(* (* print_char_matrix matrix; *) *)
(* let rows = Array.length matrix in *)
(* let cols = Array.length matrix.(0) in *)
(* printf "rows: %d cols: %d\n" rows cols; *)
(* let iter = { matrix; current_idx = 0, 0; dims = rows, cols } in *)
(* traverse iter *)

let file = In_channel.read_lines "data/3a.txt" in
let matrix = create_char_matrix file in
let rows = Array.length matrix in
let cols = Array.length matrix.(0) in
let iter = { matrix; current_idx = 0, 0; dims = rows, cols } in
traverse2 iter
