let read_lines filename =
  let file = open_in filename in
  let rec imp acc = 
    try imp (input_line file :: acc)
    with End_of_file -> close_in file; List.rev acc
  in
  imp []
;;

let int_of_char' (c: char) =
  (int_of_char c) - 48
;;

let parse_input lines =
  lines |> List.map (fun l -> String.fold_left (fun acc c -> (int_of_char' c)::acc) [] l) |> List.map List.rev
;;


let drop n l =
  let rec imp = function
    | (0, xs) -> xs
    | (_, []) -> []
    | (n, x::xs) -> imp (n-1, xs)
  in imp (n, l)
;;

let drop_end n l = l |> List.rev |> drop n |> List.rev ;;

let max_idx l =
  let rec imp maxi maxv cur = function
    | [] -> maxi
    | x::xs -> if x > maxv then imp cur x (cur+1) xs else imp maxi maxv (cur+1) xs
  in
  imp (List.hd l) 0 0 l
;;


let joltage n l =
  let rec imp acc prev_idx = function
    | 0 -> acc
    | n ->
      let l = l |> drop prev_idx |> drop_end (n-1) in
      let idx = max_idx l in
      let v = List.nth l idx in
      imp (acc * 10 + v) (idx + prev_idx + 1) (n-1)
  in
  imp 0 0 n
;;


let imp len lines =
  lines |> List.fold_left (fun acc l -> acc + (joltage len l)) 0
;;


let () = 
  let lines = read_lines "input.txt" |> parse_input in
  lines |> imp 2  |> Printf.printf "Pt01: %i\n" ;
  lines |> imp 12 |> Printf.printf "Pt02: %i\n" ;
