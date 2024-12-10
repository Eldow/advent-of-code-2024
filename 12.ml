(* Helper to convert char to elevation *)
let elevation c =
  match c with
  | 'S' -> 1
  | 'E' -> 26
  | c -> Char.code c - Char.code 'a' + 1

(* Directions for moving: up, down, left, right *)
let directions = [(-1, 0); (1, 0); (0, -1); (0, 1)]

(* Check if a move is valid *)
let is_valid_move grid (x, y) (nx, ny) =
  let rows = Array.length grid in
  let cols = String.length grid.(0) in
  nx >= 0 && ny >= 0 && nx < rows && ny < cols &&
  elevation grid.(nx).[ny] <= elevation grid.(x).[y] + 1

(* Build the graph as an adjacency list *)
let build_graph grid =
  let rows = Array.length grid in
  let cols = String.length grid.(0) in
  let graph = Hashtbl.create (rows * cols) in
  for x = 0 to rows - 1 do
    for y = 0 to cols - 1 do
      let neighbors =
        List.filter (is_valid_move grid (x, y))
          (List.map (fun (dx, dy) -> (x + dx, y + dy)) directions)
      in
      Hashtbl.add graph (x, y) neighbors
    done
  done;
  graph

(* Find the position of a character in the grid *)
let find_position grid target =
  let rows = Array.length grid in
  let cols = String.length grid.(0) in
  let rec aux x y =
    if x >= rows then None
    else if y >= cols then aux (x + 1) 0
    else if grid.(x).[y] = target then Some (x, y)
    else aux x (y + 1)
  in aux 0 0

(* Find all positions of a given character in the grid *)
let find_all_positions grid target =
  let rows = Array.length grid in
  let cols = String.length grid.(0) in
  let rec aux x y acc =
    if x >= rows then acc
    else if y >= cols then aux (x + 1) 0 acc
    else if grid.(x).[y] = target then aux x (y + 1) ((x, y) :: acc)
    else aux x (y + 1) acc
  in aux 0 0 []

(* BFS to find the shortest path from any departure point *)
let shortest_path grid graph departures =
  match (find_position grid 'E') with
  | Some goal ->
      let queue = Queue.create () in
      let visited = Hashtbl.create (Array.length grid * String.length grid.(0)) in
      (* Enqueue all departures as starting points *)
      List.iter (fun start -> Queue.add (start, 0) queue; Hashtbl.add visited start true) departures;
      let rec bfs () =
        if Queue.is_empty queue then None
        else
          let (current, steps) = Queue.pop queue in
          if current = goal then Some steps
          else
            let neighbors = Hashtbl.find graph current in
            List.iter (fun neighbor ->
              if not (Hashtbl.mem visited neighbor) then begin
                Queue.add (neighbor, steps + 1) queue;
                Hashtbl.add visited neighbor true
              end
            ) neighbors;
            bfs ()
      in bfs ()
  | _ -> None  (* No goal found *)

let grid = [|
"abaaaaaccccccccaaaaaaaaaaccccaaaaaaaaaccccaacccccccccccccccccccccaaaaaaaaaaaccaaaaaaaaaccccccccccaaaaaaaacaaaaaaccccccccccccccccccccccccccccccccccccccccccaaaaa";
"abaaaaaaaccccccaaaaaaaaaacccccaaaaaacccccaaaacccccccccccccaacccccaaaaaaaaaaaacaaaaaaaaacccccccccaaaaaaaaaaaaaaaccccccccccccccccccccccccccccccccccccccccccccaaaa";
"abacaaaaaccccccccaaaaaacccccccaaaaaacccccaaaacccccccccccccaacccaaaaaaaaaaaaaacaaaaaaaaccccccccccaaaaaaaaaaaaaaaaccccccccccccccccccccccccccaaaccccccccccccccaaaa";
"abccaacccccccccccaaaaaaccccccaaaaaaacccccaaaacccaacccccaaaaaaaaaaaaaaaaaaaaacaaaaaaaccccccccccccacaaaaaccaaaaaaaccccccccccccccccccccccccccaaccccccccccccccaaaaa";
"abcaaccccccccaaaaaaaaaaccccccaaacaaaccccccccccccaaaaaccaaaaaaaaaaaaaaaaaaaaaccaccaaacccccccccccccccaaaacccaaaaaaccccccccccccccccccccccccccaaacccccccccccccaaaca";
"abcccccccccccaaaaaaaaaaaaacccccccccacccccccccccaaaaacccccaaaacaaaaaaaaaaaaccccaaaaaaccccccccaaacccccaaccccaacccccccccccccccccccccccaaaaccaaaccccccccccccccccccc";
"abccccccccccaaaaaaccccaaaacccccccccccccccccccccaaaaaaccccaaaaaaaaaaaaaacaaacccaaaaaaaacccccaaaaaacccccccccccccccccccccccccccccccccccaaaaaaaaacccccccccccccccccc";
"abacccccccccaaaaaacccccaaacaaacccccccccacccaaccccaaaacccaaacaaaaaaaaaaacccccccaaaaaaaacccccaaaaaaccccccccccccccccccccccccccccccccjjjjjjjaaaaaaaaaccccaacccccccc";
"abaccccccccccaaaaaccaaaaaaaaaaccaccccccaacaaacccaaccccccaacccaaaaaaaaaaacccccccaaaaaaacccccaaaaaccccccccccccccccccccccccccccccccijjjjjjjjjhhhhhhhhhcaaaaaaccccc";
"abaacccccccccaaaccccaaaaaaaaaaaaaccccccaaaaacccccccccccccccccccccaaaaaaacccccccaaaaaccccccccaaaaacccccccccccccccaaaaccccccccccciijjjjjjjjjhhhhhhhhhhcaaaaaccccc";
"abaaccccccccccccccccccaaaaaacaaaaaacccccaaaaaacccccccccccaaacccccaaaccccccccccaaaaaaccccccccaacaacccccccccccccccaaaaccccccccccciiioooooojjhhhhpphhhhhaaaaaccccc";
"abacccccccccccccccccccaaaaaaccaaaaacccaaaaaaaacccccccccccaaaaaaccaacccccccccccccccaaccccccccacccccccccccccccccccaaaaccccccccccciiioooooooooopppppphiiaaaaaacccc";
"abaccccccccccaaaaaccccaaaaaaaaaaaaccccaaaaaaaacccccccccaaaaaaaaccccccccccccaaaaccccccccccccaaacccccccccccccccccccaaccccccccccciiinnoouuooooopppppppiiaaaaaacccc";
"abcccccccccccaaaaaccccaaacaaaaccaacccccccaaccccccccccccaaaaaaaaccccccccccccaaaaccccccccaaacaaacccccccccccccccccccccccccccccccciiinnotuuuuoopuuuupppiiaaacaccccc";
"abccccccccccaaaaaaccccaccccccccccccccccccaaccccccccccccaaaaaaacccccccaaacccaaaaccccccccaaaaaaaaaacccaacccccccccccccccccccccccciiinntttuuuuuuuuuuuppiiiaaccccccc";
"abaaccccccccaaaaaaccccccccccccccccccaaaacccccccccccccccccaaaaaacccccaaaaccccaaccccccccccaaaaaaaaacccaaacaaacaaaccccccccccaaccciiinntttxxuuuuuuuuuppiiiccccccccc";
"abaacccccccccaaaaaccccccccccccccccccaaaaccccccccaccccccccaaaaaacccccaaaaccccccccccccccccccaaaaacccccaaaaaaacaaaacccccccccaaaaiiiinnttxxxxuuyyyuvvppiiiccccccccc";
"abaacccccccccaaaccccccccccccccccccccaaaaccaaacaaaacccccccaaccccccccccaaacccccccccccccccccaaaaaaccccccaaaaaacaaaacccccccccaaaaiiinnnttxxxxxxyyyvvppqiiiccccccccc";
"abaccccccccccccccccccccaaccccccccccccaacccaaaaaaaacccccccccccccccccccccccccccccccccccaacaaaaaaacccaaaaaaaaccaaaccccccccaaaaahhhinnntttxxxxxyyyvvqqqiiiccccccccc";
"abcccccccccccccccccccaaaaaaccccccccccccccccaaaaaaaaaccccccccccccccccccccccccccccccaaaaaaaaacaaacccaaaaaaaaaccccccccccccaaaaahhhnnnttttxxxxyyyvvvqqqiiiccccccccc";
"SbcccccccccccccccccccaaaaaaccccccccccccccccaaaaaaaaaccccccccccccccccccccccccccccccaaaaacccccccacccaaaaaaaaaacccccccccccccaahhhnnntttxxxEzzzyyyvvvqqqjjjcccccccc";
"abcccccccccccccccccccaaaaacccccccccccccaaaaaaaaaaaacccccccccccccccccccccccccccccccaaaaaaaccccccccccccaaacaaacccccccccccccahhhmmmtttxxxxxyyyyyyyvvvqqqjjjccccccc";
"abccccccccccccccccccccaaaaacccccccccaacaaaaaaaaaaaaccccaccaaaccccccccccccccccccccaaaaaaaaccccccccccccaaacccccccccccccaaccahhhmmmtttxxxyywyyyyyyvvvqqqjjjccccccc";
"abccccccccccccccccccccaaaaacccccccccaaaaaaaacaaacccccccaaaaaaccccaccaaaccccccccccaaaaaaaaccccccccccccaacccccccccccccaaaccchhhmmmsssxxwwwyyywyyvvvvqqqjjjccccccc";
"abccaacccccccccccccccccccccccccccccccaaaaaaccaaacccccccaaaaaaccccaacaaacccccccccccacaaacccccccccccccccccccccccccaaaaaaaccchhhmmmssssswwwwyywwvvvvvqqqjjjdcccccc";
"abccaaaccaaccccccccccccccccccccccccaaaaaaaaccaaccccccccaaaaaaacccaaaaaccccccccccccccaaacccccccccccccccccccccccccaaaaaaaaaahhhmmmmsssssswwywwwrvvqqqqqjjjdddcccc";
"abccaaaaaaacccccaaaccccccccccccccccaaaaacaacccccccccccaaaaaaaaccccaaaaaaccccccccaaaccccccccccccccccccccccccccccccaaaaaaaaahhhgmmmmmsssswwwwwrrrrrqqqjjjjdddcccc";
"abcccaaaaaacccccaaacacccccccccccccccccaaaccaacccccccccaaaaaaaaccaaaaaaaacaaccccaaaacccccccccccccccccccccccccccccccaaaaaaaccggggmmmmmmssswwwwrrrrrkjjjjjddddcccc";
"abaaaaaaaaccccaacaaaaaccccaaacccccccccaaaccaaccccccccccccaaaccccaaaaacaaaaaccccaaaacccccccccccaacccccccccccaaccccaaaaaacccccgggggmmmllssswwrrrkkkkkjjjddddacccc";
"abaaaaaaaaacccaaaaaaaaccccaaaacccccccccaaaaaaccccccccccccaaacccccccaacccaaacaaacaaaccccccccccaaaacccccccaaaaaacccaaaaaaacccccgggggglllsssrrrrrkkkkkkdddddaacccc";
"abaaaaaaaaaacccaaaaaccccccaaaaccccccccccaaaaaaaccccccaaccccccccccccaaaaaaaaaaaaccccccccccccccaaaacccccccaaaaaccccaaccaaacccccccggggglllsrrrrrkkkeeedddddaaccccc";
"abaacaaaaaaaccccaaaaacccccaaaccccccccccccaaaaaaccccaaaaccccccccccccccaaaaaaaaacccccccccccccccaaaacccccccaaaaaaacccccccaacccccccccgggglllrrrrkkkeeeeeddaaaaccccc";
"abaaaaaacccccccaaacaacccccccccccccccccccaaaaaccccccaaaaaaccccccccaaacccaaaaacccccccccccccccccccccccccccaaaaaaaacccccccccccccccccccggfllllllkkkeeeeeccaaaaaacccc";
"abaaaaacccccccccaacccccccccccccccccccccaaaaaacccccccaaaacccccacccaaccccaaaaaaccccccccccccccccccccccccccaaaaaaaacccccccccccaacccccccffflllllkkkeeeccccaaaaaacccc";
"abaaaaacccccccccccccccccccccccccccaacccccccaaccccccaaaaacccccaaaaaaaccaaaaaaaaaaccccccccccccccccccccccccacaaacccccccccaaccaaccccccccfffllllkeeeecccccaacccccccc";
"abaaaacccccccccccccccccccccccccccaaacccccccccccccccaacaacccccaaaaaaccaaaaacaaaaaccccccccccccccccccccccccccaaacccccccccaaaaaaccccccccffffffffeeeeccccccccccccccc";
"abaaaccccccccccccccccccccccccccccaaaaacacccccccccccccccccccccaaaaaaaaaaaaccaaaaaaaaccccccaaccccccccccaaaaacccccccccccccaaaaaaacccccccfffffffeeaaccccccccccccaaa";
"abaacccccaacaacccccccccccccaacaaaaaaaaaacccccccaaccccccccccccaaaaaaaaaaacccaaaaaaaacccccaaacaacccccccaaaaaccccccccccaaccaaaaaaccccccccafffffeaacccccccccccccaaa";
"abaaaccccaaaaacccccccccccccaacaaaaaaaaaaccccccaaaccccccccccccaaaaaaaaaaaccccaaaaaccccccccaaaaaccccccaaaaaacccccccaacaaaaaaaaccccccccccaaacccccccccccccccccccaaa";
"abccccccccaaaaacccccccccaaaaaaaaaaaaaacccccaaaaacaaccccccaaaaaaaaaaaaaaaaaaaaaaaaacccccaaaaaacccccccaaaaaacccccccaaaaaaaacaaccccccccccaaaccccccccccccccccaaaaaa";
"abcccccccaaaaaacccccccccaaaaaaaaaaaaaacccccaaaaaaaaccccccaaaaacaaaaaaaaaaaaaaaaaaacccccaaaaaaaacccccaaaaaaccccccaaaaaaaaccaacccccccccccccccccccccccccccccaaaaaa";
|]

let () =
    (* Build the graph *)
  let graph = build_graph grid in

  (* Find all positions with elevation 'S' and check the shortest path *)
  let departures_s = find_all_positions grid 'S' in  (* all positions with elevation 'S' *)
  (match shortest_path grid graph departures_s with
    | Some steps -> Printf.printf "Shortest path (S): %d\n" steps
    | None -> Printf.printf "No path found for 'S'\n");

  (* Find all positions with elevation 'a' and check the shortest path *)
  let departures_a = find_all_positions grid 'a' in  (* all positions with elevation 'a' *)
  (match shortest_path grid graph departures_a with
    | Some steps -> Printf.printf "Shortest path (multi departures): %d\n" steps
    | None -> Printf.printf "No path found for 'a'\n")
