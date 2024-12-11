let compute_distance list1 list2 =
  let sorted1 = List.sort compare list1 in
  let sorted2 = List.sort compare list2 in
  List.fold_left2 (fun acc x y -> acc + abs (x - y)) 0 sorted1 sorted2

let () =
 let list_a = [1; 2; 3; 4] in
 let list_b = [2; 3; 4; 5] in
 let distance = compute_distance list_a list_b in
 Printf.printf "Distance: %d" distance
