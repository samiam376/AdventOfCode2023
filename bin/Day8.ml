open Core

type direction =
  | Left
  | Right
[@@deriving sexp]

let pattern = Re2.create_exn {|\w{3}|}

let find_values s =
  let values = Re2.find_all_exn pattern s in
  values
;;

let parse =
  let lines = In_channel.read_lines "data/8.txt" in
  let first = List.hd_exn lines in
  let rest = List.tl_exn lines in
  let directions =
    first
    |> String.to_list
    |> List.map ~f:(fun c ->
      match c with
      | 'L' -> Left
      | 'R' -> Right
      | _ -> failwith "bad")
  in
  let coords = List.map rest ~f:find_values in
  directions, coords
;;

let get_value map key direction =
  let value = Map.find_exn map key in
  match direction with
  | Left -> fst value
  | Right -> snd value
;;

let get_directions all_directions remaining_directions =
  match remaining_directions with
  | [] -> all_directions
  | _ -> remaining_directions
;;

let search directions map =
  let rec iter key count remaining_directions =
    match key with
    | "ZZZ" -> count
    | _ ->
      let directions = get_directions directions remaining_directions in
      let next_key = get_value map key (List.hd_exn directions) in
      iter next_key (count + 1) (List.tl_exn directions)
  in
  iter "AAA" 0 directions
;;

let ends_with s c = Char.equal s.[String.length s - 1] c
let rec gcd a b = if b = 0 then a else gcd b (a mod b)
let lcm a b = if a = 0 || b = 0 then 0 else a * b / gcd a b

(* the paths are cyclic so we need to find when they all end at the same time*)
let search2 directions map =
  let starts = Map.filter_keys ~f:(fun k -> ends_with k 'A') map |> Map.keys in
  List.iter starts ~f:print_endline;
  let rec iter key count remaining_directions =
    let is_end = ends_with key 'Z' in
    if is_end
    then count
    else (
      let directions = get_directions directions remaining_directions in
      let next_key = get_value map key (List.hd_exn directions) in
      iter next_key (count + 1) (List.tl_exn directions))
  in
  let counts = List.map starts ~f:(fun s -> iter s 0 directions) in
  List.iter counts ~f:(printf "%d\n");
  let final = List.fold counts ~init:1 ~f:lcm in
  final
;;

let () =
  let directions, map = parse in
  let cord_map =
    List.fold map ~init:String.Map.empty ~f:(fun acc l ->
      match l with
      | [ a; b; c ] -> Map.add_exn acc ~key:a ~data:(b, c)
      | _ -> failwith "bad map input")
  in
  let count = search2 directions cord_map in
  printf "count: %d\n" count;
  print_endline "done"
;;
