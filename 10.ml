module CoordSet = Set.Make(struct
  type t = int * int
  let compare = compare
end)

(* Gets all neighbours as a list of tuples *)
let get_neighbors (x, y) max_x max_y =
  let moves = [(0, 1); (1, 0); (0, -1); (-1, 0)] in
  let potential_neighbors = List.map (fun (dx, dy) -> (x + dx, y + dy)) moves in
  let valid_neighbors =
    List.filter
      (fun (nx, ny) -> nx >= 0 && ny >= 0 && nx < max_x && ny < max_y)
      potential_neighbors
  in
  valid_neighbors

(* Recursive DFS to collect trailheads from a single departure point *)
let rec dfs map visited (x, y) =
  let max_x = Array.length map in
  let max_y = Array.length map.(0) in
  if List.mem (x, y) visited then CoordSet.empty (* Skip already visited cells for this path *)
  else
    let visited = (x, y) :: visited in
    let current_value = map.(x).(y) in
    if current_value = 9 then CoordSet.singleton (x, y) (* Found a trailhead! *)
    else
      let neighbors = get_neighbors (x, y) max_x max_y in
      List.fold_left
        (fun acc (nx, ny) ->
           if map.(nx).(ny) = current_value + 1 then
             CoordSet.union acc (dfs map visited (nx, ny))
           else acc)
        CoordSet.empty
        neighbors

(* Function to count the total number of trailheads, considering multiple departures *)
let count_trailheads map =
  let max_x = Array.length map in
  let max_y = Array.length map.(0) in
  let trailheads = ref 0 in

  (* Iterate through all cells to find all departure points (0s) *)
  for x = 0 to max_x - 1 do
    for y = 0 to max_y - 1 do
      if map.(x).(y) = 0 then
        (* Collect trailheads from each departure point and count them *)
        let trailheads_from_departure = dfs map [] (x, y) in
        trailheads := !trailheads + (CoordSet.cardinal trailheads_from_departure)
    done
  done;

  (* Return the total number of trailheads *)
  !trailheads

let map = [|
  [| 0; 1; 0; 3; |];
  [| 1; 2; 3; 4; |];
  [| 8; 9; 6; 5; |];
  [| 9; 8; 7; 6; |]
|]

let () =
  let trailheads = count_trailheads map in
  Printf.printf "Number of trailheads: %d\n" trailheads
