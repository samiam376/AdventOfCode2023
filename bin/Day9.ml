open Core

let parse =
  let lines = In_channel.read_lines "data/9.txt" in
  let values =
    List.map lines ~f:(fun l -> l |> String.split ~on:' ' |> List.map ~f:Int.of_string)
  in
  values
;;

let print_list l =
  List.iter l ~f:(fun x -> printf "%d " x);
  printf "\n"
;;

let list_diffs l =
  let rec aux acc = function
    | [] | [ _ ] -> acc
    | a :: b :: tl -> aux ((b - a) :: acc) (b :: tl)
  in
  aux [] l
;;

let is_zeroes list = List.for_all list ~f:(fun x -> x = 0)
let last_element l = List.last_exn l

let rec find_num l acc =
  let diff = list_diffs l in
  (* printf "diff: "; *)
  (* print_list diff; *)
  if is_zeroes diff then acc else acc + find_num (List.rev diff) (List.hd_exn diff)
;;

let rec find_num2 l acc =
  let diff = list_diffs l in
  printf "diff: ";
  print_list diff;
  let reversed = List.rev diff in
  let hd = List.hd_exn reversed in
  printf "hd: %d\n" hd;
  if is_zeroes diff then acc else acc - find_num2 reversed (List.hd_exn reversed)
;;

(* let values = parse in *)
(* let decoded = *)
(*   List.map values ~f:(fun l -> *)
(*     let num = find_num l 0 in *)
(*     let last = last_element l in *)
(*     (* printf "last: %d, num: %d\n" last num; *) *)
(*     last + num) *)
(* in *)
(* let total = List.fold decoded ~init:0 ~f:( + ) in *)
(* printf "total: %d\n" total *)
(* List.iter decoded ~f:(fun x -> printf "%d\n" x) *)
let values = parse in
let decoded =
  List.map values ~f:(fun l ->
    let num = find_num2 l 0 in
    let first = List.hd_exn l in
    printf "first: %d, num: %d\n" first num;
    first + num)
in
let total = List.fold decoded ~init:0 ~f:( + ) in
printf "total: %d\n" total
