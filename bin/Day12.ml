open Core

let lines = In_channel.read_lines "data/12.txt"

let data =
  List.map lines ~f:(String.split ~on:' ')
  |> List.map ~f:(function
    | [ springs; counts ] ->
      let parsed_springs = String.to_array springs in
      let parsed_counts = counts |> String.split ~on:',' |> List.map ~f:Int.of_string in
      parsed_springs, parsed_counts
    | _ -> failwith "unexpcted structure")
;;

let rec powerset = function
  | [] ->
    [ [] ]
    (* Base case: The powerset of an empty list is a list containing the empty list *)
  | x :: xs ->
    let ps = powerset xs in
    ps @ List.map ~f:(fun set -> x :: set) ps (* Add x to each subset of the powerset *)
;;

let print_power ps =
  printf "powerset\n";
  List.iter ps ~f:(fun l -> List.iter l ~f:(fun i -> printf "%d " i));
  printf "\n"
;;

let is_valid groups counts =
  if List.length groups <> List.length counts
  then false
  else (
    let valid_groups =
      List.map2_exn groups counts ~f:(fun g c ->
        let len = String.length g in
        Int.equal len c)
    in
    List.for_all valid_groups ~f:(fun g -> g))
;;

let unkown_idxs springs =
  Array.foldi springs ~init:[] ~f:(fun idx acc v ->
    if Char.equal v '?' then idx :: acc else acc)
;;

let print_int_list numbers =
  List.iter ~f:(fun num -> printf "%d " num) numbers;
  printf "\n"
;;

let group_exp = Re2.create_exn {|#+|}
let find_groups s = Re2.find_all_exn group_exp s

let solution =
  List.map data ~f:(fun (springs, counts) ->
    let unkown_idxs = unkown_idxs springs in
    (* printf "unkown_idxs: \n"; *)
    (* print_int_list unkown_idxs; *)
    let powerset = powerset unkown_idxs in
    (* printf "powerset len: %d\n" (List.length powerset); *)
    (* print_power powerset; *)
    let arrangements =
      List.map powerset ~f:(fun s ->
        if List.length s = 0
        then false
        else (
          (* printf "powerset: \n"; *)
          (* print_int_list s; *)
          let replaced =
            Array.mapi springs ~f:(fun i a ->
              let is_unkown = List.mem s i ~equal:Int.equal in
              if is_unkown then '#' else if Char.equal a '?' then '.' else a)
          in
          let groups =
            replaced
            |> String.of_array
            |> (fun s ->
                 printf "%s s" s;
                 s)
            |> find_groups
          in
          (* List.iter groups ~f:(fun g -> printf "group %s" g); *)
          is_valid groups counts))
    in
    List.fold arrangements ~init:0 ~f:(fun acc b -> if b then acc + 1 else acc))
;;

let total = List.fold solution ~init:0 ~f:( + );;

printf "solution: %d" total
