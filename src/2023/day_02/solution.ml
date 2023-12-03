let read_lines filename =
  let file = open_in filename in
  let rec imp acc = 
    try imp (input_line file :: acc)
    with End_of_file -> close_in file; List.rev acc
  in
  imp []
;;


type draw = {red: int; green: int; blue: int}

type game = {id: int; draws: draw list}


let split_list = function
  | [] -> failwith ""
  | x::[] -> failwith ""
  | x::xs -> (x, xs)
;;


let rec first_where p = function
  | [] -> None
  | x::xs -> if p x then Some x else first_where p xs
;;


let rec count_where p = function
  | [] -> 0
  | x::xs -> (if p x then 1 else 0) + count_where p xs
;;

let rec all p = function
  | [] -> true
  | x::xs -> p x && all p xs
;;


let parse_single_draw input =
  let input = input |> List.map (String.split_on_char ' ') in
  let imp col = (match first_where (fun x -> List.nth x 1 = col) input with | None -> 0 | Some x -> int_of_string (List.hd x)) in
  {red = imp "red"; green = imp "green"; blue = imp "blue"}
;;

let parse_game text =
  let (prefix, suffix) = split_list (String.split_on_char ':' text) in
  let id = int_of_string (List.nth (String.split_on_char ' ' prefix) 1) in
  let draws = (String.split_on_char ';' (List.hd suffix))
    |> List.map (fun x -> (String.split_on_char ',') x |> List.map String.trim)
    |> List.map parse_single_draw
  in
  {id; draws}
;;

let pt1 games =
  let possible {red; green; blue} = red <= 12 && green <= 13 && blue <= 14 in
  games
    |> List.filter (fun {id; draws} -> all possible draws)
    |> List.map (fun {id; _} -> id)
    |> List.fold_left (+) 0
;;

let identity x = x ;;

let pt2 games =
  let max' draws =
    let rec imp r g b = function
      | [] -> (r, g, b)
      | ({red; green; blue})::xs -> imp (max r red) (max g green) (max b blue) xs
    in
    imp 0 0 0 draws
  in
  games
    |> List.map (fun {id; draws} -> max' draws)
    |> List.map (fun (red, green, blue) -> red * green * blue)
    |> List.fold_left (+) 0
;;


let () = 
  let games = read_lines "input.txt" |> List.map parse_game in
  games |> pt1 |> Printf.printf "Pt01: %i\n" ;
  games |> pt2 |> Printf.printf "Pt02: %i\n" ;
;;