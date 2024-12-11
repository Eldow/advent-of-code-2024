let is_safe_report reports =
  let rec check_safe direction = function
    | [] | [_] -> true  (* If the list is empty or has one element, it's safe *)
    | x :: y :: rest ->
      let diff = abs (x - y) in
      if diff > 3 then
        false  (* Unsafe if the difference is greater than 3 *)
      else
        if diff = 0 then
          false  (* Unsafe if the numbers are the same *)
        else
          match direction with
          | Some true ->  (* Already increasing *)
              if x >= y then false (* If it's not strictly increasing, it's unsafe *)
              else check_safe (Some true) (y :: rest)
          | Some false -> (* Already decreasing *)
              if x <= y then false (* If it's not strictly decreasing, it's unsafe *)
              else check_safe (Some false) (y :: rest)
          | None ->  (* First pair, decide the direction *)
              if x < y then check_safe (Some true) (y :: rest)
              else if x > y then check_safe (Some false) (y :: rest)
              else false  (* If x == y, it's unsafe *)
  in
  check_safe None reports

(* Test function to check if removing exactly one element makes the report safe *)
let is_safe_with_one_removal report =
  let n = List.length report in
  let rec try_removal idx =
    if idx >= n then false  (* If we tried removing all elements, return false *)
    else
      let modified_report = List.filteri (fun i _ -> i <> idx) report in
      if is_safe_report modified_report then true
      else try_removal (idx + 1)  (* Try removing the next element *)
  in
  try_removal 0  (* Start by trying to remove the first element *)

(* Test function to count safe reports in a list of lists of reports considering one removal *)
let count_safe_reports report_lists =
  let count = List.fold_left (fun acc report ->
    if is_safe_report report || is_safe_with_one_removal report then acc + 1 else acc
  ) 0 report_lists in
  count

(* Test cases *)
let () =
  let reports = [
    [7; 6; 4; 2; 1];  (* Safe *)
    [1; 2; 7; 8; 9];  (* Unsafe *)
    [9; 7; 6; 2; 1];  (* Unsafe *)
    [1; 3; 2; 4; 5];  (* Unsafe - Safe with 1 removal *)
    [8; 6; 4; 4; 1];  (* Unsafe - Safe with 1 removal *)
    [1; 3; 6; 7; 9];  (* Safe *)
    [9; 7; 5; 3; 1];  (* Safe *)
    [1; 2; 3; 5; 6];  (* Safe *)
    [1; 2; 3; 4; 6];  (* Safe *)
    [1; 3; 6; 8; 10]; (* Safe *)
  ]; in
  let safe_count = count_safe_reports reports in
  Printf.printf "Number of safe reports: %d\n" safe_count
