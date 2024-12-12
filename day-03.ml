
(* Import the Re module for regular expression operations *)
#use "topfind";;
#require "re";;
open Re.Str

(* Function to parse the string *)
let parse_with_pause str =
  (* Regular expressions *)
  let re_mul = regexp "mul(\\([0-9]+\\),\\([0-9]+\\))" in
  let re_dont = regexp "don't()" in
  let re_do = regexp "do()" in

  (* Helper function to process matches *)
  let rec aux idx paused accum =
    if idx >= String.length str then accum
    else
      if string_match re_dont str idx then
        (* Pause parsing *)
        aux (match_end ()) true accum
      else if string_match re_do str idx then
        (* Resume parsing *)
        aux (match_end ()) false accum
      else if (not paused) && string_match re_mul str idx then
        (* Add match if not paused *)
        let left = matched_group 1 str |> int_of_string in
        let right = matched_group 2 str |> int_of_string in
        aux (match_end ()) paused (accum + (left * right))
      else
        (* Skip to the next character *)
        aux (idx + 1) paused accum
  in

  (* Start parsing from the beginning, unpaused *)
  aux 0 false 0

let () =
  let input = "some random text mul(10,20) more text mul(-5, 30)" in
  let total_sum = parse_with_pause input in
  Printf.printf "The total sum of multiplications is: %d\n" total_sum
