open Base
open Stdio

let is_digit = function
  | '0' .. '9' -> true
  | _ -> false
;;

let number_strings =
  [ "one", "1"
  ; "two", "2"
  ; "three", "3"
  ; "four", "4"
  ; "five", "5"
  ; "six", "6"
  ; "seven", "7"
  ; "eight", "8"
  ; "nine", "9"
  ]
;;

let find_prefix s =
  List.find number_strings ~f:(fun (p, _) -> String.is_prefix s ~prefix:p)
;;

let next_substring s = String.sub s ~pos:1 ~len:(String.length s - 1)

let rec extract_numbers s acc =
  if String.is_empty s
  then List.rev acc
  else (
    match find_prefix s with
    | Some p ->
      let remaining = next_substring s in
      extract_numbers remaining (snd p :: acc)
    | None ->
      let first_char = s.[0] in
      if is_digit first_char
      then (
        let remaining = next_substring s in
        extract_numbers remaining (Char.to_string first_char :: acc))
      else (
        let remaining = next_substring s in
        extract_numbers remaining acc))
;;

let replace_number_strings s = extract_numbers s [] |> String.concat

let replace_number_strings s =
  printf "-------\n";
  printf "before : %s\n" s;
  let after = replace_number_strings s in
  printf "after : %s\n" after;
  printf "-------\n";
  after
;;

let get_last l = List.rev l |> List.hd

let first_and_last l =
  match List.hd l, get_last l with
  | Some first, Some last -> first, last
  | _ -> failwith "empty list"
;;

let chars_to_int (first, second) =
  let first = Char.get_digit first in
  let second = Char.get_digit second in
  match first, second with
  | Some first, Some second -> (first * 10) + second
  | _ -> failwith "not a digit"
;;

let extract_digits s =
  replace_number_strings s |> String.to_list |> first_and_last |> chars_to_int
;;

let read_file file =
  let rec aux file acc =
    let line = In_channel.input_line file in
    match line with
    | None -> List.rev acc
    | Some line -> aux file (line :: acc)
  in
  aux file []
;;

let file = In_channel.create "data/1.txt" in
let lines = read_file file in
lines |> List.map ~f:extract_digits |> List.fold ~init:0 ~f:( + ) |> printf "Sum: %d\n"
