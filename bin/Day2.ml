open Core
open Angstrom

(* let capture_game s = *)
(*   let regex_pattern = Str.regexp {|\(Game\([0-9]+\)\):|} in *)
(*   if Str.string_match regex_pattern s 0 *)
(*   then ( *)
(*     let game = Str.matched_group 2 s in *)
(*     let remaining = Str.global_replace regex_pattern "" s in *)
(*     Int.of_string game, remaining) *)
(*   else raise (Failure "no match game") *)
(* ;; *)

type color =
  | Red of int
  | Green of int
  | Blue of int

let ws = skip_while Char.is_whitespace
let int = take_while1 Char.is_digit >>| Int.of_string

let color_parser : color Angstrom.t =
  ws *> int
  <* ws
  >>= fun i ->
  string "red" *> return (Red i)
  <|> string "green" *> return (Green i)
  <|> string "blue" *> return (Blue i)
  <* ws
;;

let color_list_parser : color list Angstrom.t = sep_by (char ',') color_parser

let color_list_list_parser : color list list Angstrom.t =
  sep_by (char ';') color_list_parser
;;

let game_parser : (int * color list list) Angstrom.t =
  let gp = ws *> string "Game" <* ws >>= fun _ -> int <* ws <* char ':' in
  let cp = ws *> color_list_list_parser in
  gp >>= fun i -> cp >>= fun c -> return (i, c)
;;

(* let test_color_list_parser = *)
(*   let test_string = "5 blue, 6 red, 7 green; 1 red, 6 blue" in *)
(*   match Angstrom.parse_string ~consume:All color_list_list_parser test_string with *)
(*   | Ok color -> *)
(*     List.iter color ~f:(fun l -> *)
(*       List.iter l ~f:(fun c -> *)
(*         match c with *)
(*         | Red i -> printf "red: %d\n" i *)
(*         | Green i -> printf "green: %d\n" i *)
(*         | Blue i -> printf "blue: %d\n" i)) *)
(*   | Error msg -> failwith msg *)
(* ;; *)

let parse str =
  match Angstrom.parse_string ~consume:All (sep_by (string " ") game_parser) str with
  | Ok parsed_game -> List.hd_exn parsed_game
  | Error msg -> failwith msg
;;

(* let capture_color_count s = *)
(*   let pattern = Str.regexp {|\([0-9]+\)\([a-z]+\)|} in *)
(*   if Str.string_match pattern s 0 *)
(*   then ( *)
(*     let count = Str.matched_group 1 s in *)
(*     let color = Str.matched_group 2 s in *)
(*     count, color) *)
(*   else raise (Failure "no color count match") *)
(* ;; *)

(* let parse_color_count (count, color) = *)
(*   let count = Int.of_string count in *)
(*   match color with *)
(*   | "red" -> Red count *)
(*   | "green" -> Green count *)
(*   | "blue" -> Blue count *)
(*   | _ -> raise (Failure ("invalid color: " ^ color)) *)
(* ;; *)

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

(* let parse_reveal s = *)
(*   let colors = String.split ~on:',' s in *)
(*   let parsed_colors = *)
(*     List.map colors ~f:(fun c -> c |> capture_color_count |> parse_color_count) *)
(*   in *)
(*   let reveal = create_reveal parsed_colors in *)
(*   reveal *)
(* ;; *)

let parse_game s =
  let game, draws = parse s in
  let reveals = List.map draws ~f:create_reveal in
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

(* Example usage *)
(* let () = *)
(*   let input_string = "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green" in *)
(*   let parsed_game = parse input_string in *)
(*   let game = List.hd_exn parsed_game in *)
(*   printf "game: %d " (fst game); *)
(*   List.iter (snd game) ~f:(fun l -> *)
(*     List.iter l ~f:(fun x -> *)
(*       match x with *)
(*       | Green i -> printf "green: %d " i *)
(*       | Blue i -> printf "blue: %d " i *)
(*       | Red i -> printf "red: %d " i)) *)
(* ;; *)
