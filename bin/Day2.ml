open Core

let capture_game s =
  let regex_pattern = Str.regexp {|\(Game\([0-9]+\)\):|} in
  if Str.string_match regex_pattern s 0
  then (
    let game = Str.matched_group 2 s in
    let remaining = Str.global_replace regex_pattern "" s in
    Int.of_string game, remaining)
  else raise (Failure "no match game")
;;

type color =
  | Red of int
  | Green of int
  | Blue of int

let capture_color_count s =
  let pattern = Str.regexp {|\([0-9]+\)\([a-z]+\)|} in
  if Str.string_match pattern s 0
  then (
    let count = Str.matched_group 1 s in
    let color = Str.matched_group 2 s in
    count, color)
  else raise (Failure "no color count match")
;;

let parse_color_count (count, color) =
  let count = Int.of_string count in
  match color with
  | "red" -> Red count
  | "green" -> Green count
  | "blue" -> Blue count
  | _ -> raise (Failure ("invalid color: " ^ color))
;;

type reveal =
  { red : int
  ; green : int
  ; blue : int
  }

let create_reveal colors =
  let empty_reveal = { red = 0; green = 0; blue = 0 } in
  List.fold colors ~init:empty_reveal ~f:(fun acc color ->
    match color with
    | Red count -> { acc with red = count }
    | Blue count -> { acc with blue = count }
    | Green count -> { acc with green = count })
;;

let is_valid_reveal reveal ~red ~green ~blue =
  reveal.red <= red && reveal.green <= green && reveal.blue <= blue
;;

let is_valid_game reveals ~red ~green ~blue =
  List.for_all reveals ~f:(fun r -> is_valid_reveal r ~red ~green ~blue)
;;

let calculate_power reveals =
  let empty_reveal = { red = 0; green = 0; blue = 0 } in
  let bag =
    List.fold reveals ~init:empty_reveal ~f:(fun acc reveal ->
      { red = max acc.red reveal.red
      ; green = max acc.green reveal.green
      ; blue = max acc.blue reveal.blue
      })
  in
  let power = bag.red * bag.green * bag.blue in
  (* printf "red: %d, green: %d, blue: %d, power: %d\n" bag.red bag.green bag.blue power; *)
  power
;;

let parse_reveal s =
  let colors = String.split ~on:',' s in
  let parsed_colors =
    List.map colors ~f:(fun c -> c |> capture_color_count |> parse_color_count)
  in
  let reveal = create_reveal parsed_colors in
  reveal
;;

let parse_game s =
  let cleaned = String.substr_replace_all s ~pattern:" " ~with_:"" in
  let game, remaining_string = capture_game cleaned in
  let reveals = String.split ~on:';' remaining_string |> List.map ~f:parse_reveal in
  game, reveals
;;

(* let _part1 = *)
(*   let file = In_channel.read_lines "data/2a.txt" in *)
(*   let games = List.map file ~f:parse_game in *)
(*   let valid_games = *)
(*     List.filter games ~f:(fun game -> is_valid_game (snd game) ~red:12 ~blue:14 ~green:13) *)
(*   in *)
(*   let total = List.fold valid_games ~init:0 ~f:(fun acc (game, _) -> acc + game) in *)
(*   printf "%d\n" total *)
(* ;; *)

let _part2 =
  let file = In_channel.read_lines "data/2a.txt" in
  let games = List.map file ~f:parse_game in
  let powers = List.map games ~f:(fun (_, reveals) -> calculate_power reveals) in
  let total = List.fold powers ~init:0 ~f:( + ) in
  printf "%d\n" total
;;
