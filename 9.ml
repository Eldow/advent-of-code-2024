(* Parse the input string into a list of integers *)
let parse_input input =
  let rec aux acc i =
    if i >= String.length input then List.rev acc
    else aux (int_of_char input.[i] - int_of_char '0' :: acc) (i + 1)
  in
  aux [] 0

(* Build the initial memory layout *)
let build_memory sizes =
  let rec aux acc id = function
    | [] -> List.rev acc
    | size :: rest ->
      let segment = if id mod 2 = 0 then List.init size (fun _ -> id / 2) else List.init size (fun _ -> -1) in
      aux (List.rev_append segment acc) (id + 1) rest
  in
  aux [] 0 sizes

(* Utility to take k elements from a list *)
let rec take k list = match k with
  | 0 -> []
  | k -> match list with
          | [] -> failwith "take"
          | y::ys -> y :: (take (k - 1) ys)

(* Compact the memory while adhering to the two rules and trimming excess free spaces *)
let compact_memory memory =
  (* Helper function to compact memory while shifting the end *)
  let rec compact_aux processed unprocessed remaining =
    match unprocessed, remaining with
    | [], _ -> List.rev processed (* All blocks processed; return compacted memory *)
    | -1 :: xs, y :: ys -> (* Fill a free space with the rightmost allocated block *)
      compact_aux (y :: processed) xs ys  (* Move 'y' from remaining to processed, and remove it from remaining *)
    | x :: xs, _ -> (* Keep allocated blocks in their original positions *)
      compact_aux (x :: processed) xs remaining
  in

  (* Extract allocated blocks *)
  let alloc_list =
    List.filter (fun x -> x <> -1) memory |> List.rev
  in

  (* Start the compaction process *)
  compact_aux [] (take (List.length alloc_list) memory) alloc_list

(* Calculate the checksum *)
let calculate_checksum memory =
  List.mapi (fun index id -> if id = -1 then 0 else index * id) memory
  |> List.fold_left ( + ) 0

(* Main function *)
let () =
  let input = "2333133121414131402" in
  let sizes = parse_input input in
  let initial_memory = build_memory sizes in
  let compacted_memory = compact_memory initial_memory in
  let checksum = calculate_checksum compacted_memory in

  (* Print the results *)
  Printf.printf "Checksum: %d\n" checksum;
