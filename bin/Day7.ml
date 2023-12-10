open Core

type card =
  | J (*pt 2 now joker *)
  | Two
  | Three
  | Four
  | Five
  | Six
  | Seven
  | Eight
  | Nine
  | T
  | Q
  | K
  | A
[@@deriving compare, sexp, equal]

module Card = struct
  type t = card [@@deriving compare, sexp]
end

module CardSet = Map.Make (Card)

type hand = card list [@@deriving sexp]

let counts =
  List.fold ~init:CardSet.empty ~f:(fun acc card ->
    Map.update acc card ~f:(fun s ->
      match s with
      | None -> 1
      | Some x -> x + 1))
;;

let j_count counts =
  match Map.find counts J with
  | None -> 0
  | Some x -> x
;;

let is_high_card counts = Map.length counts = 5
let is_one_pair counts = Map.length counts = 4

(*
   is one pair
   or
   j count = 1 and is high card
*)
let is_one_pair_j counts =
  let j_count = j_count counts in
  is_one_pair counts || (j_count = 1 && is_high_card counts)
;;

let is_two_pair counts =
  let num_of_twos = Map.filter counts ~f:(fun c -> c = 2) |> Map.length in
  num_of_twos = 2
;;

let is_three_of_a_kind counts =
  let num_of_ones = Map.filter counts ~f:(fun c -> c = 1) |> Map.length in
  Map.length counts = 3 && num_of_ones = 2
;;

let is_four_of_a_kind counts =
  Map.length counts = 2 && Map.exists counts ~f:(fun v -> v = 4)
;;

let is_five_of_a_kind counts = Map.length counts = 1

let is_full_house counts =
  Map.length counts = 2
  && Map.exists counts ~f:(fun v -> v = 3)
  && Map.exists counts ~f:(fun v -> v = 2)
;;

type hand_rank =
  | HighCard
  | OnePair
  | TwoPair
  | ThreeOfAKind
  | FullHouse
  | FourOfAKind
  | FiveOfAKind
[@@deriving compare, sexp]

let find_rank hand =
  let counts = counts hand in
  if is_five_of_a_kind counts
  then FiveOfAKind
  else if is_four_of_a_kind counts
  then FourOfAKind
  else if is_full_house counts
  then FullHouse
  else if is_three_of_a_kind counts
  then ThreeOfAKind
  else if is_two_pair counts
  then TwoPair
  else if is_one_pair counts
  then OnePair
  else HighCard
;;

let best_rank hand =
  let counts = counts hand in
  let joker_count = j_count counts in
  let distinct_non_jacks =
    Map.filter_keys counts ~f:(fun k -> not (equal_card k J)) |> Map.length
  in
  match joker_count, distinct_non_jacks with
  | 0, _ -> find_rank hand
  | 1, 1 -> FiveOfAKind (* J, 1 1 1 1 *)
  | 1, 2 ->
    if is_three_of_a_kind counts
    then FourOfAKind
    else FullHouse (* J 1 1 2 2 | J 1 1 1 2*)
  | 1, 3 -> ThreeOfAKind (* J 1 1 2 3 *)
  | 1, 4 -> OnePair (* J 1 2 3 4*)
  | 2, 1 -> FiveOfAKind (* J J 1 1 1*)
  | 2, 2 -> FourOfAKind (* J J 1 1 2*)
  | 2, 3 -> ThreeOfAKind (* J J 1 2 3*)
  | 3, 1 -> FiveOfAKind (* J J J 1 1*)
  | 3, 2 -> FourOfAKind (* J J J 1 2*)
  | 4, 1 -> FiveOfAKind (* J J J J 1*)
  | 5, 0 -> FiveOfAKind
  | _ -> failwith "invalid hand"
;;

let rec compare_high_card (lista : hand) (listb : hand) =
  match lista, listb with
  | [ a ], [ b ] -> Card.compare a b
  | hd_a :: tl_a, hd_b :: tl_b ->
    let cmp = Card.compare hd_a hd_b in
    if cmp = 0 then compare_high_card tl_a tl_b else cmp
  | _ -> failwith "invalid hand"
;;

let compare_hands handa handb =
  let rank_a = best_rank handa in
  let rank_b = best_rank handb in
  let compared = compare_hand_rank rank_a rank_b in
  if compared = 0 then compare_high_card handa handb else compared
;;

let file = In_channel.read_lines "data/7.txt" in
let hands =
  List.map file ~f:(fun line ->
    let split = String.split line ~on:' ' in
    let bid = List.nth_exn split 1 |> Int.of_string in
    let hand =
      List.hd_exn split
      |> String.to_list
      |> List.map ~f:(fun c ->
        match c with
        | 'A' -> A
        | 'K' -> K
        | 'Q' -> Q
        | 'J' -> J
        | 'T' -> T
        | '9' -> Nine
        | '8' -> Eight
        | '7' -> Seven
        | '6' -> Six
        | '5' -> Five
        | '4' -> Four
        | '3' -> Three
        | '2' -> Two
        | c -> failwith ("invalid card: " ^ Char.to_string c))
    in
    hand, bid)
in
let sorted = List.sort hands ~compare:(fun (h_a, _) (h_b, _) -> compare_hands h_a h_b) in
let total =
  List.foldi sorted ~init:0 ~f:(fun idx acc (hand, bid) ->
    let rank = idx + 1 in
    let hand_rank = best_rank hand in
    let hand_rank_string = hand_rank |> sexp_of_hand_rank |> Sexp.to_string_hum in
    let hand_string = hand |> sexp_of_hand |> Sexp.to_string_hum in
    printf
      "hand: %s, hand_rank: %s, rank: %d, bid: %d, acc: %d\n"
      hand_string
      hand_rank_string
      rank
      bid
      acc;
    (rank * bid) + acc)
in
printf "%d\n" total
(* in *)
(* List.iter hands ~f:(fun (hand, _bid) -> *)
(*   hand |> sexp_of_hand |> Sexp.to_string_hum |> print_endline) *)
