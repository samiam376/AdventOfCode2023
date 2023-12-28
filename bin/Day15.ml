open Core

let input = In_channel.read_all "data/15.txt"

let codes =
  input |> String.substr_replace_all ~pattern:"\n" ~with_:"" |> String.split ~on:','
;;

let hash s =
  s
  |> String.to_list
  |> List.fold ~init:0 ~f:(fun acc c ->
    let ascii = Char.to_int c in
    (acc + ascii) * 17 mod 256)
;;

(*pt 1*)
(* let total = *)
(*   List.fold codes ~init:0 ~f:(fun acc curr -> *)
(*     let h = hash curr in *)
(*     (* printf "curr: %s, hash: %d\n" curr h; *) *)
(*     acc + h) *)
(* ;; *)

(* printf "total: %d\n" total *)

(*pt 2*)

type operation =
  | Add of int
  | Remove

let parse_operation s =
  let rec aux charlist label =
    match charlist with
    | [] -> failwith "empty list"
    | hd :: tl when Char.equal hd '=' ->
      let lens = tl |> String.of_list |> Int.of_string in
      let label_string = label |> List.rev |> String.of_list in
      label_string, Add lens
    | [ '-' ] ->
      let label_string = label |> List.rev |> String.of_list in
      label_string, Remove
    | hd :: tl -> aux tl (hd :: label)
  in
  let label_string, op = aux (String.to_list s) [] in
  let box = hash label_string in
  box, label_string, op
;;

let boxes = Array.create ~len:256 []
let operations = List.map codes ~f:(fun code -> parse_operation code)
let label_exists list label = List.exists list ~f:(fun (l, _) -> String.equal l label)

let update_existing list (label, fl) =
  List.map list ~f:(fun (l2, fl2) -> if String.equal l2 label then l2, fl else l2, fl2)
;;

List.iter operations ~f:(fun (box, label, op) ->
  match op with
  | Add lens ->
    let current_list = boxes.(box) in
    let updated_list =
      if label_exists current_list label
      then update_existing current_list (label, lens)
      else (label, lens) :: current_list
    in
    boxes.(box) <- updated_list
  | Remove ->
    let current_list = boxes.(box) in
    let filtered_list =
      List.filter current_list ~f:(fun (l, _) -> not (String.equal l label))
    in
    boxes.(box) <- filtered_list)

let total_power =
  Array.foldi boxes ~init:0 ~f:(fun box_idx acc lenses ->
    let lens_total =
      lenses
      |> List.rev
      |> List.foldi ~init:0 ~f:(fun idx acc (label, focal_len) ->
        let box = box_idx + 1 in
        let slot = idx + 1 in
        let total = box * slot * focal_len in
        printf
          "label: %s, box: %d, slot: %d, fl: %d, total: %d\n"
          label
          box
          slot
          focal_len
          total;
        acc + total)
    in
    acc + lens_total)
;;

printf "total power: %d\n" total_power
