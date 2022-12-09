open Stdio

exception Wrong_Range
let range_of_str str = 
  match String.split_on_char '-' str with
  | f::s::[] -> (int_of_string f, int_of_string s)
  | _ -> raise Wrong_Range

exception Wrong_Pair
let pair_of_ranges str =
  match String.split_on_char ',' str with
  | f::s::[] -> (range_of_str f, range_of_str s)
  | _ -> raise Wrong_Pair

let contains (range1, range2) =
  match range1, range2 with
  | (l1, r1), (l2, r2) when l1<=l2 && r1>=r2 -> true
  | (l1, r1), (l2, r2) when l1>=l2 && r1<=r2 -> true
  | _ -> false

let task1 content =
  print_string "Task 1: ";
  content |> List.map pair_of_ranges |> List.filter contains |> List.length |> print_int |> print_newline

let overlap (range1, range2) =
  match range1, range2 with
  | (l1, r1), (l2, r2) when (l1<=l2 && l2<=r1) || (l1<=r2 && r2<=r1) -> true
  | (l1, r1), (l2, r2) when (l2<=l1 && l1<=r2) || (l2<=r1 && r1<=r2) -> true
  | _ -> false

let task2 content =
  print_string "Task 2: ";
  content |> List.map pair_of_ranges |> List.filter overlap |> List.length |> print_int |> print_newline

let file = 
  match Array.length Sys.argv with
  | 1 -> "input.txt"
  | _ -> Sys.argv.(1)

let () = 
  let content = open_in file |> In_channel.input_lines in
  task1 content; task2 content

(* dune exec --display quiet --no-print-directory ./day4.exe [filename] *)