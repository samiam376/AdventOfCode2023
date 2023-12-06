open Core
open Angstrom

let ws = skip_while Char.is_whitespace
let int = take_while1 Char.is_digit >>| Int.of_string
let parse_sublist = sep_by (skip_while Char.is_whitespace) int

let parser =
  string "Card " *> ws *> int
  <* ws
  <* char ':'
  <* ws
  >>= fun num -> sep_by (ws *> char '|' <* ws) parse_sublist >>= fun l -> return (num, l)
;;

let parse s =
  match parse_string ~consume:Prefix parser s with
  | Ok v -> v
  | Error e -> failwith e
;;

let pow base exp =
  let rec aux base exp acc = if exp = 0 then acc else aux base (exp - 1) (acc * base) in
  aux base exp 1
;;

let score winning mine =
  let winning_numbers = Int.Set.of_list winning in
  let my_winning = List.filter mine ~f:(fun x -> Set.mem winning_numbers x) in
  let total =
    match my_winning with
    | [] -> 0
    | _hd :: tl ->
      let tail_len = List.length tl in
      pow 2 tail_len
  in
  total
;;

(* let lines = In_channel.read_lines "data/4a.txt" in *)
(* let games = List.map lines ~f:parse in *)
(* let total = *)
(*   List.fold games ~init:0 ~f:(fun acc game -> *)
(*     let _, values = game in *)
(*     let win = List.hd_exn values in *)
(*     let mine = List.nth_exn values 1 in *)
(*     acc + score win mine) *)
(* in *)
(* printf "total: %d\n" total *)

let matching_num_count winning mine =
  let winning_numbers = Int.Set.of_list winning in
  let my_winning = List.filter mine ~f:(fun x -> Set.mem winning_numbers x) in
  List.length my_winning
;;

let update_score score_map game_number =
  Map.update score_map game_number ~f:(fun s ->
    match s with
    | None -> 1
    | Some v -> v + 1)
;;

let gen_inclusive_range start stop = List.range start ~stop:`inclusive stop

let rec count2 game_number game_map score_map =
  let winning, mine = Map.find_exn game_map game_number in
  let matches = matching_num_count winning mine in
  if matches = 0
  then score_map
  else (
    let range = gen_inclusive_range 1 matches in
    printf
      "game_number: %d, matches: %d, range: %s"
      game_number
      matches
      (List.to_string range ~f:Int.to_string);
    let score_map =
      List.fold range ~init:score_map ~f:(fun acc i ->
        let next_score = count2 (game_number + i) game_map acc in
        update_score next_score game_number)
    in
    score_map)
;;

let count game_number game_map score_map = count2 game_number game_map score_map;;

(*
   --- Part Two ---

   Just as you're about to report your findings to the Elf, one of you realizes that the rules have actually been printed on the back of every card this whole time.

   There's no such thing as "points". Instead, scratchcards only cause you to win more scratchcards equal to the number of winning numbers you have.

   Specifically, you win copies of the scratchcards below the winning card equal to the number of matches. So, if card 10 were to have 5 matching numbers, you would win one copy each of cards 11, 12, 13, 14, and 15.

   Copies of scratchcards are scored like normal scratchcards and have the same card number as the card they copied. So, if you win a copy of card 10 and it has 5 matching numbers, it would then win a copy of the same cards that the original card 10 won: cards 11, 12, 13, 14, and 15. This process repeats until none of the copies cause you to win any more cards. (Cards will never make you copy a card past the end of the table.)

   This time, the above example goes differently:

   Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53
   Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19
   Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1
   Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83
   Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36
   Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11

   Card 1 has four matching numbers, so you win one copy each of the next four cards: cards 2, 3, 4, and 5.
   Your original card 2 has two matching numbers, so you win one copy each of cards 3 and 4.
   Your copy of card 2 also wins one copy each of cards 3 and 4.
   Your four instances of card 3 (one original and three copies) have two matching numbers, so you win four copies each of cards 4 and 5.
   Your eight instances of card 4 (one original and seven copies) have one matching number, so you win eight copies of card 5.
   Your fourteen instances of card 5 (one original and thirteen copies) have no matching numbers and win no more cards.
   Your one instance of card 6 (one original) has no matching numbers and wins no more cards.

   Once all of the originals and copies have been processed, you end up with 1 instance of card 1, 2 instances of card 2, 4 instances of card 3, 8 instances of card 4, 14 instances of card 5, and 1 instance of card 6. In total, this example pile of scratchcards causes you to ultimately have 30 scratchcards!
*)

let lines = In_channel.read_lines "data/4a.txt" in
let games = List.map lines ~f:parse in
let games_alist =
  List.map games ~f:(fun game ->
    let num, values = game in
    let win = List.hd_exn values in
    let mine = List.nth_exn values 1 in
    num, (win, mine))
in
let game_map = Int.Map.of_alist_exn games_alist in
let score_map = Int.Map.empty in
let scores =
  Map.fold game_map ~init:score_map ~f:(fun ~key:game ~data:_ acc ->
    count game game_map acc)
in
let len = Map.length game_map in
let total = len + Map.fold scores ~init:0 ~f:(fun ~key:_ ~data:v acc -> acc + v) in
printf "total: %d\n" total
