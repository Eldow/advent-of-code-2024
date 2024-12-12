(* Helper function to parse the grid into a 2D array *)
let parse_grid grid = Array.of_list (List.map Array.of_list grid)

(* Directions for neighbors: up, down, left, right *)
let directions = [(-1, 0); (1, 0); (0, -1); (0, 1)]

(* Check if a coordinate is within bounds *)
let in_bounds grid (x, y) =
  x >= 0 && y >= 0 && x < Array.length grid && y < Array.length grid.(0)

(* Helper function to classify a cell's state based on its neighbors *)
let classify_cell grid (x, y) =
  let neighbors = List.map (fun (dx, dy) ->
    let nx, ny = x + dx, y + dy in
    if in_bounds grid (nx, ny) && grid.(nx).(ny) = grid.(x).(y) then 'X'
    else 'O'
  ) directions in
  neighbors

(* Count corners based on the 8 neighbors around the cell *)
let count_corners grid (x, y) =
  let neighbor_offsets = [
    (-1, -1); (-1, 0); (-1, 1); (* Top-left, Top, Top-right *)
    (0, -1);         (0, 1);   (* Left, Right *)
    (1, -1); (1, 0); (1, 1)    (* Bottom-left, Bottom, Bottom-right *)
  ] in

  let neighbors = Array.of_list (List.map (fun (dx, dy) ->
    let nx, ny = x + dx, y + dy in
    if in_bounds grid (nx, ny) && grid.(nx).(ny) = grid.(x).(y) then 'X' else 'O'
  ) neighbor_offsets) in

  (* Analyze the 8 neighbors to count corners *)
  let corner_count = ref 0 in

  (* Check specific corner configurations using array indexing *)
  if neighbors.(1) = 'O' && neighbors.(3) = 'O' then incr corner_count; (* Top-left *)
  if neighbors.(1) = 'X' && neighbors.(3) = 'X' && neighbors.(0) = 'O'  (* Top-left inner *)
    then incr corner_count;

  if neighbors.(1) = 'O' && neighbors.(4) = 'O' then incr corner_count; (* Top-right *)
  if neighbors.(1) = 'X' && neighbors.(4) = 'X' && neighbors.(2) = 'O'  (* Top-right inner *)
    then incr corner_count;

  if neighbors.(3) = 'O' && neighbors.(6) = 'O' then incr corner_count; (* Bottom-left *)
  if neighbors.(3) = 'X' && neighbors.(6) = 'X' && neighbors.(5) = 'O'  (* Bottom-left inner *)
    then incr corner_count;

  if neighbors.(4) = 'O' && neighbors.(6) = 'O' then incr corner_count; (* Bottom-right *)
  if neighbors.(4) = 'X' && neighbors.(6) = 'X' && neighbors.(7) = 'O'  (* Bottom-right inner *)
    then incr corner_count;

  !corner_count

(* Perform a flood fill to compute the area, perimeter, and corner count *)
let flood_fill grid visited (start_x, start_y) =
  let id = grid.(start_x).(start_y) in
  let queue = Queue.create () in
  let area = ref 0 in
  let perimeter = ref 0 in
  let corners = ref 0 in
  Queue.add (start_x, start_y) queue;
  visited.(start_x).(start_y) <- true;
  while not (Queue.is_empty queue) do
    let (x, y) = Queue.pop queue in
    incr area;
    (* Count corners for this cell *)
    let corner_count = count_corners grid (x, y) in
    corners := !corners + corner_count;

    (* Enqueue unvisited neighbors *)
    List.iter (fun (dx, dy) ->
      let nx, ny = x + dx, y + dy in
      if not (in_bounds grid (nx, ny)) || grid.(nx).(ny) <> id then
        incr perimeter
      else if not visited.(nx).(ny) then begin
        visited.(nx).(ny) <- true;
        Queue.add (nx, ny) queue
      end
    ) directions
  done;
  (!area, !perimeter, !corners)

(* Compute the total cost of fencing all plots in the grid *)
let compute_fencing_cost grid =
  let grid = parse_grid grid in
  let rows, cols = Array.length grid, Array.length grid.(0) in
  let visited = Array.make_matrix rows cols false in
  let total_cost = ref 0 in
  let reduced_cost = ref 0 in
  for x = 0 to rows - 1 do
    for y = 0 to cols - 1 do
      if not visited.(x).(y) then
        let area, perimeter, corners = flood_fill grid visited (x, y) in
        total_cost := !total_cost + (area * perimeter);
        reduced_cost := !reduced_cost + (area * corners);
    done
  done;
  !total_cost, !reduced_cost

(* Example grid *)
let grid = [
  ['R'; 'R'; 'R'; 'R'; 'I'; 'I'; 'C'; 'C'; 'F'; 'F'];
  ['R'; 'R'; 'R'; 'R'; 'I'; 'I'; 'C'; 'C'; 'C'; 'F'];
  ['V'; 'V'; 'R'; 'R'; 'R'; 'C'; 'C'; 'F'; 'F'; 'F'];
  ['V'; 'V'; 'R'; 'C'; 'C'; 'C'; 'J'; 'F'; 'F'; 'F'];
  ['V'; 'V'; 'V'; 'V'; 'C'; 'J'; 'J'; 'C'; 'F'; 'E'];
  ['V'; 'V'; 'I'; 'V'; 'C'; 'C'; 'J'; 'J'; 'E'; 'E'];
  ['V'; 'V'; 'I'; 'I'; 'I'; 'C'; 'J'; 'J'; 'E'; 'E'];
  ['M'; 'I'; 'I'; 'I'; 'I'; 'I'; 'J'; 'J'; 'E'; 'E'];
  ['M'; 'I'; 'I'; 'I'; 'S'; 'I'; 'J'; 'E'; 'E'; 'E'];
  ['M'; 'M'; 'M'; 'I'; 'S'; 'S'; 'J'; 'E'; 'E'; 'E']
]

(* Compute and print the cost *)
let () =
  let cost, r_cost = compute_fencing_cost grid in
  Printf.printf "Total cost of fencing: %d\nReduced cost of fencing: %d\n" cost r_cost
