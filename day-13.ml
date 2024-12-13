type coordinates = { x: int; y: int }
type entry = { button_a: coordinates; button_b: coordinates; prize: coordinates }

(* Parsing bullshit *)
let parse_button line =
  Scanf.sscanf line "X+%d, Y+%d" (fun x y -> { x; y })

let parse_prize line =
  Scanf.sscanf line "X=%d, Y=%d" (fun x y -> { x; y })

let print_line line = Printf.printf "Line %s\n" line

let parse_entry (lines: string list) : entry =
  match lines with
  | [a; b; p] ->
      let button_a = parse_button (String.sub a 10 (String.length a - 10)) in
      let button_b = parse_button (String.sub b 10 (String.length b - 10)) in
      let prize = parse_prize (String.sub p 7 (String.length p - 7)) in
      { button_a; button_b; prize }
  | _ -> failwith "Invalid entry format"

let split_entries (input: string) : string list list =
  let lines : string list = String.split_on_char '\n' input |> List.map String.trim in
  let rec group_entries (acc: string list list) (current: string list) (lines: string list) : string list list =
    match lines with
    | [] -> current :: acc
    | "" :: rest -> group_entries (current :: acc) [] rest
    | line :: rest -> group_entries acc (line :: current) rest
  in
  group_entries [] [] lines

let parse_input (input: string) : entry list =
  input
  |> split_entries
  |> List.filter (fun l -> List.length l > 0)
  |> List.map List.rev
  |> List.map parse_entry

(* A function to compute the cost given a certain number of presses for buttons A and B *)
let compute_cost a_presses b_presses =
  3 * a_presses + b_presses  (* Button A costs 3 tokens, Button B costs 1 token *)

(* Function to solve the system using matrix algebra and check divisibility *)
let solve_system_with_cost entry offset =
  let { button_a; button_b; prize } = entry in
  (* Compute the determinant and other necessary components *)
  let det = button_a.x * button_b.y - button_a.y * button_b.x in
  let a = button_b.y * (prize.x + offset) - button_b.x * (prize.y + offset) in
  let b = button_a.x * (prize.y + offset) - button_a.y * (prize.x + offset) in

  (* Check if both a and b are divisible by the determinant *)
  if (3 * a) mod det = 0 && b mod det = 0 then
    (compute_cost a b) / det
  else
    0

(* Aggregate the costs *)
let count_total_cost entries offset: int =
  List.fold_left (fun acc entry -> acc + solve_system_with_cost entry offset) 0 entries

let input = "
Button A: X+94, Y+34
Button B: X+22, Y+67
Prize: X=8400, Y=5400

Button A: X+26, Y+66
Button B: X+67, Y+21
Prize: X=12748, Y=12176

Button A: X+17, Y+86
Button B: X+84, Y+37
Prize: X=7870, Y=6450

Button A: X+69, Y+23
Button B: X+27, Y+71
Prize: X=18641, Y=10279
"

let () =
  let entries = parse_input input in
  let cost = count_total_cost entries 0 in
  Printf.printf "Cost: %d\n" cost;
  let cost_offset = count_total_cost entries 10000000000000 in
  Printf.printf "Cost with offset: %d\n" cost_offset
