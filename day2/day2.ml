type sign = Rock | Paper | Scissors

exception WrongSign 

let sign_of_char = function
  | 'A' | 'X'  -> Rock
  | 'B' | 'Y' -> Paper
  | 'C' | 'Z' -> Scissors
  | _ -> raise WrongSign

let winner = function
  | Rock -> Paper
  | Paper -> Scissors
  | Scissors -> Rock

let loser = function
  | Rock -> Scissors
  | Paper -> Rock
  | Scissors -> Paper

let choose_sign sn = function
  | 'X' -> loser sn
  | 'Y' -> sn
  | 'Z' -> winner sn
  | _ -> raise WrongSign

let signs_of_pair1 (a, b) = (sign_of_char a, sign_of_char b)

let signs_of_pair2 (a, b) = 
  let op = sign_of_char a in
  let my = choose_sign op b in
  (op, my)

let parse_file filename =
  let channel = Stdlib.open_in filename in
  let rec read_pair acc =
    try
      match input_line channel with
      | "" -> acc
      | str -> read_pair ((str.[0], str.[2])::acc)
    with
    | End_of_file -> acc
  in
  read_pair []

let score_match = function
  | Rock, Paper | Paper, Scissors | Scissors, Rock -> 6
  | a, b when a = b -> 3
  | _, _ -> 0

let score_sign = function
  | Rock -> 1
  | Paper -> 2
  | Scissors -> 3

let score_points (a,b) = score_sign b + score_match (a,b)

let (%) f g x = x |> f |> g

let () =
  let content = "input.txt" |> parse_file in
  print_string "task1: ";
  content |> List.map (signs_of_pair1 % score_points) |> List.fold_left (+) 0 |> print_int |> print_newline;
  print_string "task2: ";
  content |> List.map (signs_of_pair2 % score_points) |> List.fold_left (+) 0 |> print_int