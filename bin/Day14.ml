open Core

let data = In_channel.read_lines "data/14.txt"
let grid = List.map data ~f:(fun s -> String.to_array s) |> Array.of_list

let print_array arr =
  Array.iter arr ~f:(fun row ->
    Array.iter row ~f:(fun col -> printf "%c" col);
    printf "\n")
;;

let rows = Array.length grid
let cols = Array.length grid.(0);;

printf "Initial Grid \n";
print_array grid

let can_roll above = Char.equal above '.'
let is_o value = Char.equal value 'O'

let () =
  for r = 1 to rows - 1 do
    for c = 0 to cols - 1 do
      for row_above = r - 1 downto 0 do
        let current_value = grid.(row_above + 1).(c) in
        let value_above = grid.(row_above).(c) in
        (* printf "current value: %c, value above: %c\n" current_value value_above; *)
        if is_o current_value && can_roll value_above
        then (
          (* printf "rolling\n"; *)
          grid.(row_above).(c) <- current_value;
          grid.(row_above + 1).(c) <- value_above)
        else ()
      done
    done
  done
;;

printf "rolled grid\n";
print_array grid
