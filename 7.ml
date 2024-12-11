(* Function to evaluate a list of numbers with a corresponding list of operators *)
let rec evaluate nums ops =
  match nums, ops with
  | n :: [], [] -> Some n  (* If there are no more operators, return the single number *)
  | n1 :: n2 :: ns, op :: os ->
      let result = match op with
        | '+' -> n1 + n2
        | '*' -> n1 * n2
        | 'u' -> int_of_string (string_of_int n1 ^ string_of_int n2)
        | _ -> failwith "Invalid operator"
      in
      evaluate (result :: ns) os
  | _ -> None (* If lengths don't match, return None *)

(* Generate all combinations of +, *, and || operators for a given list length *)
let rec generate_ops length =
  if length = 0 then [[]]
  else
    let shorter = generate_ops (length - 1) in
    List.concat [List.map (fun ops -> '+' :: ops) shorter;
                 List.map (fun ops -> '*' :: ops) shorter;
                 List.map (fun ops -> 'u' :: ops) shorter]

(* Find valid operators for a given target and list of numbers *)
let find_valid_ops target nums =
  let num_ops = List.length nums - 1 in
  let ops_combinations = generate_ops num_ops in
  List.filter_map (fun ops ->
    match evaluate nums ops with
    | Some result when result = target -> Some ops
    | _ -> None
  ) ops_combinations

(* Parse each input line into target and numbers *)
let parse_line line =
  match String.split_on_char ':' line with
  | [target; nums] ->
      let target = int_of_string (String.trim target) in
      let nums = List.map int_of_string (String.split_on_char ' ' (String.trim nums)) in
      (target, nums)
  | _ -> failwith "Invalid input format"

(* Process each equation and sum solvable targets *)
let process_equations lines =
  let equations = List.map parse_line lines in
  List.fold_left (fun acc (target, nums) ->
    match find_valid_ops target nums with
    | [] -> acc  (* Skip unsolvable equations *)
    | _ -> acc + target
  ) 0 equations

let () =
  let input = [
    "190: 10 19";
    "3267: 81 40 27";
    "83: 17 5";
    "156: 15 6";
    "7290: 6 8 6 15";
    "161011: 16 10 13";
    "192: 17 8 14";
    "21037: 9 7 18 13";
    "292: 11 6 16 20"
  ] in
  let result = process_equations input in
  Printf.printf "Final sum of solvable targets: %d\n" result
