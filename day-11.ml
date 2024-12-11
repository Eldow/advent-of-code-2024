let transform_number n =
  match n with
  | 0 -> [1]  (* Rule 1: Replace 0 with 1 *)
  | _ ->
      let s = string_of_int n in
      let len = String.length s in
      if len mod 2 = 0 then
        (* Rule 2: Split number with even digits into two halves *)
        let half = len / 2 in
        let left = String.sub s 0 half |> int_of_string in
        let right = String.sub s half (len - half) |> int_of_string in
        [left; right]
      else
        (* Rule 3: Multiply by 2024 *)
        [n * 2024]

(* Apply the transformation to a list of integers *)
let transform_list lst =
  let rec aux acc = function
    | [] -> List.rev acc
    | x :: xs -> aux (List.rev_append (transform_number x) acc) xs
  in
  aux [] lst

(* Apply the transformation iteratively for n times *)
let apply_transformation n lst =
  let rec aux current_n current_list =
    if current_n <= 0 then current_list
    else aux (current_n - 1) (transform_list current_list)
  in
  aux n lst

(* Print the result *)
let () =
  let input_list = [0; 12; 123; 1000; 7] in
  let times = 75 in
  let transformed_list = apply_transformation times input_list in
  Printf.printf "Length: %d" (List.length transformed_list)
