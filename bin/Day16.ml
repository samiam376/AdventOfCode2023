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
[@@deriving sexp, equal]

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

let rec beam_imp input_cords dir seen ~shadow =
  let cords = ref input_cords in
  try
    while true do
      let row = fst !cords in
      let col = snd !cords in
      (* printf "row: %d, col %d\n" row col; *)
      let key = cord_key (row, col) dir in
      let is_inbounds = inbounds (row, col) in
      (* printf "inbounds: %b\n" is_inbounds; *)
      let has_visited = Hashtbl.mem seen key in
      (* printf "has visited: %b\n" has_visited; *)
      if is_inbounds && not has_visited
      then (
        let next = next_cords (row, col) in
        Hashtbl.add_exn seen ~key ~data:true;
        shadow.(row).(col) <- '#';
        match grid.(row).(col) with
        | '.' -> cords := next dir
        | '\\' ->
          (match dir with
           | Up -> cords := next Left
           | Down -> cords := next Right
           | Left -> cords := next Up
           | Right -> cords := next Down)
        | '/' ->
          (match dir with
           | Up -> cords := next Right
           | Down -> cords := next Left
           | Left -> cords := next Down
           | Right -> cords := next Up)
        | '|' ->
          (match dir with
           | Up | Down -> cords := next dir
           | Left | Right ->
             printf "splitting | \n";
             let up = next Up in
             beam_imp up Up seen ~shadow;
             let down = next Down in
             beam_imp down Down seen ~shadow)
        | '-' ->
          (match dir with
           | Left | Right -> cords := next dir
           | Up | Down ->
             printf "splitting -\n";
             let left = next Left in
             beam_imp left Left seen ~shadow;
             let right = next Right in
             beam_imp right Right seen ~shadow)
        | _ -> failwith "unexpected char")
      else raise Exit
    done
  with
  | Exit -> printf "exiting\n"
;;

let rec beam (row, col) dir seen ~shadow =
  let key = cord_key (row, col) dir in
  if inbounds (row, col) && not (Hashtbl.mem seen key)
  then (
    printf "at row: %d, col: %d\n" row col;
    let value = grid.(row).(col) in
    let next = next_cords (row, col) in
    Hashtbl.add_exn seen ~key ~data:true;
    shadow.(row).(col) <- '#';
    match value with
    | '.' -> beam ~shadow (next dir) dir seen
    | '|' ->
      (match dir with
       | Up | Down -> beam ~shadow (next dir) dir seen
       | Left | Right ->
         beam ~shadow (next Up) Up seen;
         beam ~shadow (next Down) Down seen)
    | '-' ->
      (match dir with
       | Left | Right -> beam ~shadow (next dir) dir seen
       | Up | Down ->
         beam ~shadow (next Right) Right seen;
         beam ~shadow (next Left) Left seen)
    | '\\' ->
      (match dir with
       | Up -> beam ~shadow (next Left) Left seen
       | Down -> beam ~shadow (next Right) Right seen
       | Left -> beam ~shadow (next Up) Up seen
       | Right -> beam ~shadow (next Down) Down seen)
    | '/' ->
      (match dir with
       | Up -> beam ~shadow (next Right) Right seen
       | Down -> beam ~shadow (next Left) Left seen
       | Left -> beam ~shadow (next Down) Down seen
       | Right -> beam ~shadow (next Up) Up seen)
    | c -> failwith ("unexpected char: " ^ String.of_char c))
  else ()
;;

let shadow_grid = Array.make_matrix ~dimx:rows ~dimy:cols '.'
let table = Hashtbl.create (module String);;

beam (0, 0) Right table ~shadow:shadow_grid;;

(* beam_imp (0, 0) Right table ~shadow:shadow_grid;; *)
printf "done\n"

let print_grid arr =
  Array.iter arr ~f:(fun row ->
    Array.iter row ~f:(fun col -> printf "%c" col);
    printf "\n")
;;

print_grid shadow_grid

let sum =
  shadow_grid
  |> Array.to_sequence
  |> Sequence.map ~f:Array.to_sequence
  |> Sequence.concat
  |> Sequence.fold ~init:0 ~f:(fun acc curr ->
    if Char.equal '#' curr then acc + 1 else acc)
;;

printf "sum: %d\n" sum
