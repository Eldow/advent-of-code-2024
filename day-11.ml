(* Function to transform a number according to the rules *)
let transform_number n =
  if n = 0 then
    [1]  (* Replace 0 with 1 *)
  else
    let s = string_of_int n in
    if (String.length s) mod 2 = 0 then
      (* If the number has an even number of digits, split it in half *)
      let mid = String.length s / 2 in
      let left = int_of_string (String.sub s 0 mid) in
      let right = int_of_string (String.sub s mid (String.length s - mid)) in
      [left; right]
    else
      (* If the number is odd, multiply it by 2024 *)
      [n * 2024]

(* Function to apply the operations for a fixed number of iterations *)
let apply_operations lst num_iterations =
  (* Initialize the frequency table *)
  let freq = Hashtbl.create 100 in
  List.iter (fun num -> Hashtbl.replace freq num ((Hashtbl.find_opt freq num |> Option.value ~default:0) + 1)) lst;

  (* Perform transformations for a fixed number of iterations *)
  for _ = 1 to num_iterations do
    let new_freq = Hashtbl.create 100 in
    Hashtbl.iter (fun num count ->
      let transformed = transform_number num in
      List.iter (fun new_num ->
        Hashtbl.replace new_freq new_num ((Hashtbl.find_opt new_freq new_num |> Option.value ~default:0) + count)
      ) transformed
    ) freq;
    (* Replace freq with the new frequency table *)
    Hashtbl.clear freq;
    Hashtbl.iter (fun num count -> Hashtbl.replace freq num count) new_freq;
  done;

  (* Calculate the total count of numbers in the final list *)
  Hashtbl.fold (fun _ count acc -> acc + count) freq 0

(* Print the result *)
let () =
  let input_list = [0; 12; 123; 1000; 7] in
  let times = 75 in
  let result = apply_operations input_list times in
  Printf.printf "Length: %d" result
