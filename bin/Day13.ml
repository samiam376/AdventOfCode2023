open Core

let data = In_channel.read_lines "data/13.txt"

(*collect each section into list*)
let mapped =
  List.fold data ~init:[ [] ] ~f:(fun acc v ->
    match acc with
    | cur_acc :: tl ->
      if String.is_empty v then [] :: cur_acc :: tl else (v :: cur_acc) :: tl
    | _ -> failwith "unexpected")
  |> List.map ~f:List.rev
  |> List.rev
;;

let print_array arr =
  Array.iter arr ~f:(fun row ->
    Array.iter row ~f:(fun col -> printf "%c" col);
    printf "\n")
;;

let arrays =
  List.map mapped ~f:(fun sl -> List.map sl ~f:String.to_array |> Array.of_list)
;;

(*
   relected idx = 2 * reflection_idx - cur_idx  + 1
   does it reflect on the column or over the column?
   -----R
   -012345678
   0..#....#. to check: 0 = 9, 1 = 8, 2 = 7, 3 = 5,``
   1..######.
   2..###.##.
   3####..###
   4...####..
   5###.##.##
   6##.####.#
   7...#..#..
   8..######.

   0|1234567891*2
   .|..#..##..#.. check: 0 = 13
   .|.#..####..#.
   .|..###..###..
   .|.##.####.##.
   .|.##......##.
   .|...######...
   .|.##.....###.
   #|#####..#####
   .|....#..#....
   #|#.#.#..#.#.#
   .|..#......#..
   .|...##..##...
   .|.#.######.#.
   #|#.#.####.#.#
   .|..##.##.##..
   #|#.#..##..#.#
   .|.####..####.
*)
let reflected_idx current_idx reflection_idx =
  reflection_idx - current_idx + reflection_idx + 1
;;

(*idea search from left to right in each row and check if its a reflection
  if a value is out of bounds its true since it doesn't need to be checked
*)
let is_reflection values reflection_idx =
  let search_range = List.range 0 ~stop:`inclusive reflection_idx in
  let len = Array.length values in
  (* printf "len: %d" len; *)
  (* printf "searching reflection idx: %d ------\n" reflection_idx; *)
  List.for_all search_range ~f:(fun search_idx ->
    let ridx = reflected_idx search_idx reflection_idx in
    let is_r =
      if ridx >= len then true else Char.equal values.(search_idx) values.(ridx)
    in
    is_r)
;;

let find_reflections array =
  let nrows = Array.length array in
  let ncols = Array.length array.(0) in
  (* printf "nrows: %d, ncols %d \n" nrows ncols; *)
  let col_idxs = List.range 0 ncols in
  let row_idxs = List.range 0 nrows in
  (* printf "------- vertical -------"; *)
  let vertical_refleciton =
    List.find col_idxs ~f:(fun cidx ->
      (* printf "is_even: %b\n" is_even; *)
      Array.for_alli array ~f:(fun _i row ->
        (* printf "searching row: %d, col: %d\n" i cidx; *)
        if cidx = ncols - 1 then false else is_reflection row cidx))
  in
  (* printf " --------- horizontal --------"; *)
  let horiztonal_reflection =
    List.find row_idxs ~f:(fun ridx ->
      (*map over each column and check if its horiztonally reflected*)
      List.for_all col_idxs ~f:(fun col_idx ->
        let column = Array.map array ~f:(fun r -> r.(col_idx)) in
        if ridx = nrows - 1 then false else is_reflection column ridx))
  in
  match vertical_refleciton, horiztonal_reflection with
  | Some v, Some h ->
    (* printf "found dims: (%d, %d) " nrows ncols; *)
    (* printf "vertical: %d, horiztonal: %d\n" v h; *)
    v + 1 + (100 * (h + 1))
  | Some v, None ->
    (* printf "found dims: (%d, %d) " nrows ncols; *)
    (* printf "vertical: %d\n" v; *)
    v + 1
  | None, Some h ->
    (* printf "found dims: (%d, %d) " nrows ncols; *)
    (* printf "horizontal: %d\n" h; *)
    100 * (h + 1)
  | None, None ->
    (* printf "none found: dims (%d, %d)\n" nrows ncols; *)
    0
;;

let reflections =
  List.map arrays ~f:(fun array ->
    let reflections = find_reflections array in
    (* printf "reflection: %d\n" reflections; *)
    reflections)
;;

let total = List.fold reflections ~init:0 ~f:( + ) in
printf "total: %d\n" total
