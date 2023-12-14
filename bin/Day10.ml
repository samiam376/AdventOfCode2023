open Core
(*
   The pipes are arranged in a two-dimensional grid of tiles:

   | is a vertical pipe connecting north and south.
   - is a horizontal pipe connecting east and west.
     L is a 90-degree bend connecting north and east.
     J is a 90-degree bend connecting north and west.
     7 is a 90-degree bend connecting south and west.
     F is a 90-degree bend connecting south and east.
     . is ground; there is no pipe in this tile.
     S is the starting position of the animal; there is a pipe on this tile, but your sketch doesn't show what shape the pipe has.

   Based on the acoustics of the animal's scurrying, you're confident the pipe that contains the animal is one large, continuous loop.

   For example, here is a square loop of pipe:

   .....
   .F-7.
   .|.|.
   .L-J.
   .....

   If the animal had entered this loop in the northwest corner, the sketch would instead look like this:

   .....
   .S-7.
   .|.|.
   .L-J.
   .....

   In the above diagram, the S tile is still a 90-degree F bend: you can tell because of how the adjacent pipes connect to it.

   Unfortunately, there are also many pipes that aren't connected to the loop! This sketch shows the same loop as above:

   -L|F7
   7S-7|
   L|7||
   -L-J|
   L|-JF

   In the above diagram, you can still figure out which pipes form the main loop: they're the ones connected to S, pipes those pipes connect to, pipes those pipes connect to, and so on. Every pipe in the main loop connects to its two neighbors (including S, which will have exactly two pipes connecting to it, and which is assumed to connect back to those two pipes).

   Here is a sketch that contains a slightly more complex main loop:

   ..F7.
   .FJ|.
   SJ.L7
   |F--J
   LJ...

   Here's the same example sketch with the extra, non-main-loop pipe tiles also shown:

   7-F7-
   .FJ|7
   SJLL7
   |F--J
   LJ.LJ

   If you want to get out ahead of the animal, you should find the tile in the loop that is farthest from the starting position. Because the animal is in the pipe, it doesn't make sense to measure this by direct distance. Instead, you need to find the tile that would take the longest number of steps along the loop to reach from the starting point - regardless of which way around the loop the animal went.

   In the first example with the square loop:

   .....
   .S-7.
   .|.|.
   .L-J.
   .....

   You can count the distance each tile in the loop is from the starting point like this:

   .....
   .012.
   .1.3.
   .234.
   .....

   In this example, the farthest point from the start is 4 steps away.

   Here's the more complex loop again:

   ..F7.
   .FJ|.
   SJ.L7
   |F--J
   LJ...

   Here are the distances for each tile on that loop:

   ..45.
   .236.
   01.78
   14567
   23...

   Find the single giant loop starting at S. How many steps along the loop does it take to get from the starting position to the point farthest from the starting position?
*)

type direction =
  | North
  | South
  | East
  | West
[@@deriving sexp]

(*given current direction and next char what is the next direciton to take*)
let next_direction = function
  | North, 'S' -> Some North
  | North, '|' -> Some North
  | North, '7' -> Some West
  | North, 'F' -> Some East
  | South, 'S' -> Some South
  | South, '|' -> Some South
  | South, 'L' -> Some East
  | South, 'J' -> Some West
  | East, 'S' -> Some East
  | East, '-' -> Some East
  | East, '7' -> Some South
  | East, 'J' -> Some North
  | West, 'S' -> Some West
  | West, '-' -> Some West
  | West, 'L' -> Some North
  | West, 'F' -> Some South
  | _ -> None
;;

(*map matrix to 1d array to make easier*)
let idx_to_cords ~width idx = idx mod width, idx / width
let cord_to_width ~width (x, y) = (y * width) + x

let get_value ~width ~height arr (x, y) =
  if x < 0 || x >= width || y < 0 || y >= height
  then None
  else (
    let len = Array.length arr in
    let cord = cord_to_width ~width (x, y) in
    if cord < 0 || cord >= len then None else Some arr.(cord))
;;

let get_next_cord (x, y) direction =
  match direction with
  | North -> x, y - 1
  | South -> x, y + 1
  | East -> x + 1, y
  | West -> x - 1, y
;;

let rec capture_cycle ~height ~width direction array cord cycle =
  let value = get_value ~height ~width array cord in
  (* printf *)
  (*   "capturing cycle at (%d, %d) with current direction: %s\n" *)
  (*   (fst cord) *)
  (*   (snd cord) *)
  (*   (sexp_of_direction direction |> Sexp.to_string_hum); *)
  match value with
  | Some 'S' -> Some (('S', cord) :: cycle)
  | None -> None
  | Some c ->
    let next_direction = next_direction (direction, c) in
    (match next_direction with
     | Some d ->
       let next_cord = get_next_cord cord d in
       (* printf *)
       (*   "next cord: (%d, %d) with next direction: %s\n" *)
       (*   (fst next_cord) *)
       (*   (snd next_cord) *)
       (*   (sexp_of_direction d |> Sexp.to_string_hum); *)
       capture_cycle ~width ~height d array next_cord ((c, cord) :: cycle)
     | None -> Some cycle)
;;

let get_directions_to_search = function
  | North -> [ North; East; West ]
  | South -> [ South; East; West ]
  | East -> [ East; North; South ]
  | West -> [ West; North; South ]
;;

let start_search ~height ~width array cord =
  let found_cycles =
    List.map [ North; South; East; West ] ~f:(fun d ->
      let start = get_next_cord cord d in
      printf "starting search at (%d, %d)\n" (fst start) (snd start);
      let found_cycles = capture_cycle ~height ~width d array start [ 'S', start ] in
      found_cycles)
  in
  let cycle_lens =
    found_cycles
    |> List.filter_opt
    |> List.map ~f:(fun c ->
      let len = (List.length c - 1) / 2 in
      len, c)
  in
  let longest_cycle =
    List.max_elt cycle_lens ~compare:(fun (a, _) (b, _) -> Int.compare a b)
  in
  longest_cycle
;;

let print_matrix m width =
  Array.iteri m ~f:(fun i c ->
    printf "%c" c;
    if (i + 1) % width = 0 then printf "\n")
;;

(*shoelace theorem formula*)
let shoelace_area cords =
  let rec sum c acc =
    match c with
    | (x1, y1) :: (x2, y2) :: tl -> sum ((x2, y2) :: tl) (acc + ((y1 + y2) * (x1 - x2)))
    | [ (x1, y1) ] ->
      let x2, y2 = List.hd_exn cords in
      acc + ((y1 + y2) * (x1 - x2))
    | _ -> failwith "invalid input"
  in
  let s = sum cords 0 in
  abs s / 2
;;

(*inverse picks theorem*)
let find_interior_points area boundary_points = area + 1 - (boundary_points / 2)

(*find where the start cord is*)
let find_start tiles = Array.findi tiles ~f:(fun _ c -> Char.equal c 'S')

(*read data and convert to array*)
let input = In_channel.read_lines "data/10.txt"
let rows = List.map input ~f:(fun s -> String.to_list s |> Array.of_list);;

let width = Array.length (List.hd_exn rows) in
let height = List.length rows in
printf "width: %d, height: %d\n" width height;
let tiles = Array.concat rows in
let start = find_start tiles in
match start with
| None -> printf "no start found\n"
| Some (idx, _) ->
  let x, y = idx_to_cords ~width idx in
  printf "start: %d, %d\n" x y;
  let result = start_search ~height ~width tiles (x, y) in
  (match result with
   | None -> printf "no result found\n"
   | Some (len, cycle) ->
     printf "length: %d\n" len;
     let boundary_points = List.length cycle in
     let cords = List.map cycle ~f:snd in
     let area = shoelace_area cords in
     let interior_points = find_interior_points area boundary_points in
     printf "area: %d, interior points: %d\n" area interior_points)
