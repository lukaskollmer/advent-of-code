let read_lines filename =
  let file = open_in filename in
  let rec imp acc = 
    try imp (input_line file :: acc)
    with End_of_file -> close_in file; List.rev acc
  in
  imp []
;;


let get_chars s =
  let rec imp l = function
    | -1 -> l
    | i  -> imp ((i, s.[i]) :: l) (i-1)
  in
  imp [] (String.length s - 1) |> List.map snd
;;


let drop n l =
  let rec imp = function
    | (0, xs) -> xs
    | (_, []) -> []
    | (n, x::xs) -> imp (n-1, xs)
  in imp (n, l)
;;


type inst = Mul of int*int | Do | Dont ;;


let rec pow a = function
  | 0 -> 1
  | 1 -> a
  | n -> 
    let b = pow a (n / 2) in
    b * b * (if n mod 2 = 0 then 1 else a)
;;


let is_digit c = c >= '0' && c <= '9' ;;

let make_int digits =
  digits |> List.rev |> List.mapi (fun i d -> d * (pow 10 i)) |> List.fold_left (+) 0
;;

let parse_args input =
  let rec read_num acc = function
    | [] -> List.rev acc
    | x::xs when is_digit x && List.length acc < 3 -> read_num (x::acc) xs
    | x::xs -> List.rev acc
  in
  let n1 = read_num [] input in
  let input = drop (List.length n1) input in
  match input with 
    | [] -> None, input
    | ','::rest -> (
      let n2 = read_num [] rest in
      let rest = drop (List.length n2) rest in
      match rest with
        | ')'::rest ->
          Some (
            make_int (n1 |> List.map (fun c -> Char.code c - Char.code '0')),
            make_int (n2 |> List.map (fun c -> Char.code c - Char.code '0'))
          ), rest
        | rest -> None, rest
    )
    | _::rest -> None, input
;;

let parse (input: char list) : (inst list) =
  let rec imp acc = function
    | [] -> List.rev acc
    | 'd'::'o'::'('::')'::rest -> imp (Do::acc) rest
    | 'd'::'o'::'n'::'\''::'t'::'('::')'::rest -> imp (Dont::acc) rest
    | 'm'::'u'::'l'::'('::rest -> (
      match parse_args rest with
        | None, input -> imp acc rest
        | Some (a, b), input -> imp (Mul (a, b) :: acc) rest
    )
    | _::rest -> imp acc rest
  in imp [] input
;;


let pt01 insts =
  insts |> List.fold_left (fun acc inst -> match inst with Mul (a, b) -> acc + a * b | Do | Dont -> acc) 0
;;

let pt02 insts =
  insts |> List.fold_left (fun (run, acc) inst -> match inst with
    | Mul (a, b) -> run, if run then acc + a * b else acc
    | Do -> true, acc
    | Dont -> false, acc
  ) (true, 0) |> snd
;;


let () =
  let insts = read_lines "input.txt" |> String.concat "\n" |> get_chars |> parse in
  insts |> pt01 |> Printf.printf "Pt01: %i\n" ;
  insts |> pt02 |> Printf.printf "Pt02: %i\n" ;
;;