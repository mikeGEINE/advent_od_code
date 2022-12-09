open Stdio

let priority c = if c < 'a' then Char.code c - 38 else Char.code c - 96

let explode s = List.init (String.length s) (String.get s)

let duplicates (left, right) =
  left |> explode |> List.filter (String.contains right) |> List.sort_uniq Char.compare

let duplicates3 (fst, snd, trd) =
  fst |> explode |> List.filter (String.contains snd) |> List.filter (String.contains trd) |> List.sort_uniq Char.compare

let priority_for_lst lst =
  lst |> List.map priority |> List.fold_left (+) 0

let split_in_halves str =
  let halflen = (String.length str)/2 in
  String.sub str 0 halflen , String.sub str halflen halflen

let group_by_3 lst =
  let rec group acc = function
    | a::b::c::tl -> group ((a,b,c)::acc) tl
    | _ -> acc
  in
  group [] lst 

let (%) f g x = x |> f |> g

let task1 content = 
  print_string "task1: ";
  content |> 
  List.map (split_in_halves % duplicates % priority_for_lst) |> List.fold_left (+) 0 |> print_int |> print_newline

let task2 content =
  print_string "task2: ";
  content |> group_by_3 |>
  List.map (duplicates3 % priority_for_lst) |> List.fold_left (+) 0 |> print_int |> print_newline

let file = 
  match Array.length Sys.argv with
  | 1 -> "input.txt"
  | _ -> Sys.argv.(1)

let () =
  let content = file|> Stdlib.open_in |> In_channel.input_lines ~fix_win_eol:false in
  task1 content; task2 content
  

(* dune exec --display quiet --no-print-directory ./day3.exe [filename] *)