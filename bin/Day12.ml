open Core

let lines = In_channel.read_lines "data/12.txt"

let data =
  List.map lines ~f:(String.split ~on:' ')
  |> List.map ~f:(function
    | [ springs; counts ] ->
      let expanded_spring =
        List.range 0 5 |> List.map ~f:(fun _ -> springs) |> String.concat ~sep:"?"
      in
      let expanded_count =
        List.range 0 5 |> List.map ~f:(fun _ -> counts) |> String.concat ~sep:","
      in
      let parsed_counts =
        expanded_count |> String.split ~on:',' |> List.map ~f:Int.of_string
      in
      String.to_list expanded_spring, parsed_counts
    | _ -> failwith "unexpcted structure")
;;

let is_period c = Char.equal c '.'
let is_question_mark c = Char.equal c '?'
let is_hashtag c = Char.equal c '#'
let all_periods l = List.for_all l ~f:is_period

type input =
  { springs : char list
  ; counts : int list
  }
[@@deriving equal, compare, sexp_of, of_sexp, hash]

module InputHashtable = Hashtbl.Make (struct
    type t = input [@@deriving equal, compare, sexp_of, of_sexp, hash]
  end)

let num_valid_solutions_non_rec num_valid_solutions input =
  let { springs; counts } = input in
  match springs, counts with
  | _, [] ->
    (*when counts is empty, check that there must be no more remaining springs for it to be valid*)
    List.mem springs '#' ~equal:Char.equal |> not |> Bool.to_int
    (*when the springs are empty, there must be no more remaining counts for it to be valid*)
  | [], _ -> List.is_empty counts |> Bool.to_int
  | cur_char :: rest_char, c when is_period cur_char ->
    num_valid_solutions { springs = rest_char; counts = c }
  | (cur_char :: _ as cur), cur_count :: rest_count when is_hashtag cur_char ->
    (* when the current char is # we need to check the following
       - there are enough remaining characters to complete the current group
       - if there are enough characters they are all ? or # (not .)
       - the next character after the group isnt # (it would be considered part of the same group if so)
       - or this is the end of the string
    *)
    (* printf "------\n"; *)
    (* printf "cur count: %d\n" cur_count; *)
    (* printf "remaining count: "; *)
    (* List.iter rest_count ~f:(fun c -> printf "%d " c); *)
    (* printf "\n"; *)
    let group =
      if List.length cur >= cur_count then Some (List.take cur cur_count) else None
    in
    let valid_group =
      match group with
      | None -> false
      | Some g ->
        (* printf "group: "; *)
        (* List.iter g ~f:(fun c -> printf "%c" c); *)
        (* printf "\n"; *)
        List.for_all g ~f:(fun ch -> not (is_period ch))
    in
    let valid_ending =
      match List.nth cur cur_count with
      | None -> true
      | Some ch -> not (is_hashtag ch)
    in
    if valid_group && valid_ending
    then (
      (* printf "valid group: %b, valid ending: %b\n" valid_group valid_ending; *)
      let remaining = List.drop cur (cur_count + 1) in
      (* printf "Remaining: "; *)
      (* List.iter remaining ~f:(fun c -> printf "%c " c); *)
      (* printf "\n"; *)
      num_valid_solutions { springs = remaining; counts = rest_count })
    else (* printf "invalid group\n"; *)
      0
  | cur_char :: rest_char, c when is_question_mark cur_char ->
    (*when the current char is a question mark we need to check if its valid as a # or as a . *)
    num_valid_solutions { springs = '.' :: rest_char; counts = c }
    + num_valid_solutions { springs = '#' :: rest_char; counts = c }
  | _, _ -> failwith "unexpected"
;;

let num_valid_solutions =
  Memo.recursive ~hashable:InputHashtable.hashable num_valid_solutions_non_rec
;;

let total =
  List.fold data ~init:0 ~f:(fun acc (springs, counts) ->
    let solutions = num_valid_solutions { springs; counts } in
    acc + solutions)
;;

(*correct is 8075*)
printf "total: %d" total
