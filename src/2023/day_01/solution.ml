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
    |> List.map (fun x -> (first_where is_numeric_letter x, last_where is_numeric_letter x))
    |> List.map (fun (x, y) -> (Option.get x, Option.get y))
    |> List.map (fun (x, y) -> (int_of_char' x) * 10 + (int_of_char' y))
    |> List.fold_left (+) 0
;;








let rec list_prefix n = function
  | [] -> []
  | x::xs -> if n = 0 then [] else x::(list_prefix (n-1) xs)
;;



let digits_mapping = [
  (1, "one");
  (2, "two");
  (3, "three");
  (4, "four");
  (5, "five");
  (6, "six");
  (7, "seven");
  (8, "eight");
  (9, "nine")
] |> List.map (fun (x, y) -> (x, get_chars y))
;;


let leading_number mapping l = 
  mapping
  |> first_where (fun (x, s) -> list_prefix (List.length s) l = s)
  |> Option.map fst
;;

let rec first_number' mapping (x::xs) =
  if is_numeric_letter x then int_of_char' x
  else match leading_number mapping (x::xs) with
    | Some i -> i
    | None -> first_number' mapping xs
;;

let first_number x = first_number' digits_mapping x ;;

let last_number x = first_number' (List.map (fun (a, b) -> (a, List.rev b)) digits_mapping) (List.rev x) ;;


let pt02 input =
  input
    |> List.map (fun x -> (first_number x, last_number x))
    |> List.map (fun (a, b) -> a * 10 + b)
    |> List.fold_left (+) 0
;;



let () =
  let input = read_lines "input.txt" |> List.map get_chars in
  input |> pt01 |> Printf.printf "Pt01 sol: %i\n" ;
  input |> pt02 |> Printf.printf "Pt02 sol: %i\n" ;
;;
