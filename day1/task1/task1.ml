let rec input_list cha acc = 
  try 
    match Stdlib.input_line cha with
    | "" -> Ok acc
    | str -> input_list cha (int_of_string str :: acc)
  with 
  | End_of_file -> Result.error "end"

let rec input_multilist acc cha =
  match input_list cha [] with
  | Ok lst -> input_multilist (lst::acc) cha
  | Error _ -> acc

let lst_input = 
  Stdlib.open_in "input.txt" |> input_multilist []


let task1 () =
  lst_input |> List.map (List.fold_left (+) 0) |> List.sort (fun x y -> ~- (compare x y)) |> List.hd |> 
    string_of_int |> print_string
  
let _ = task1 ()