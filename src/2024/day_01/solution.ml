let read_lines filename =
  let file = open_in filename in
  let rec imp acc = 
    try imp (input_line file :: acc)
    with End_of_file -> close_in file; List.rev acc
  in
  imp []
;;


let parse_input lines =
  lines |> List.map (fun l ->
    let numbers = String.split_on_char ' ' l |> List.filter ((<>) String.empty) |> List.map int_of_string in
    List.nth numbers 0, List.nth numbers 1
  )
;;


let zip x y =
  let rec imp = function
    | ([], _) | (_, []) -> []
    | (x::xs, y::ys) -> (x, y) :: imp (xs, ys)
  in
  imp (x, y)
;;


let rec count_of a = function
  | [] -> 0
  | x::xs when x=a -> 1 + count_of a xs
  | _::xs -> count_of a xs
;;


let pt01 input =
  let fst_col = input |> List.map fst |> List.sort compare in
  let snd_col = input |> List.map snd |> List.sort compare in
  let paired = zip fst_col snd_col in
  paired |> List.fold_left (fun acc (x, y) -> acc + ((max x y) - (min x y))) 0
;;


let pt02 input =
  let fst_col = input |> List.map fst in
  let snd_col = input |> List.map snd in
  fst_col |> List.fold_left (fun acc x ->
    acc + x * (count_of x snd_col)
  ) 0
;;


let () =
  let input = read_lines "input.txt" |> parse_input in
  input |> pt01 |> Printf.printf "Pt01: %i\n" ;
  input |> pt02 |> Printf.printf "Pt02: %i\n" ;
