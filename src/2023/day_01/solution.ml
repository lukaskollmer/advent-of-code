let read_lines filename =
  let file = open_in filename in
  let rec imp acc = 
    try imp (input_line file :: acc)
    with End_of_file -> close_in file; List.rev acc
  in
  imp []
;;


let rec first_where p = function
  | [] -> None
  | x::xs -> if p x then Some x else first_where p xs
;;

let last_where p l = first_where p (List.rev l) ;;

let is_numeric_letter c = c >= '0' && c <= '9' ;;
let int_of_char' c = int_of_char c - int_of_char '0' ;;

let get_chars s =
  let rec imp l = function
    | -1 -> l
    | i  -> imp (s.[i] :: l) (i-1)
  in
  imp [] (String.length s - 1)
;;



let pt01 input =
  input
    |> List.map get_chars
    |> List.map (fun x -> (first_where is_numeric_letter x, last_where is_numeric_letter x))
    |> List.map (fun (x, y) -> (Option.get x, Option.get y))
    |> List.map (fun (x, y) -> (int_of_char' x) * 10 + (int_of_char' y))
    |> List.fold_left (+) 0
;;




let () =
  let input = read_lines "input.txt" in
  input |> pt01 |> Printf.printf "Pt01 sol: %i\n"
;;
