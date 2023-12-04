module BaseList = List


(* Insert Isabelle's OCaml output here *)


let read_lines filename =
  let file = open_in filename in
  let rec imp acc = 
    try imp (input_line file :: acc)
    with End_of_file -> close_in file; BaseList.rev acc
  in
  imp []
;;

let get_chars s =
  let rec imp l = function
    | -1 -> l
    | i  -> imp (s.[i] :: l) (i-1)
  in
  imp [] (String.length s - 1)
;;


let rec int_of_nat = function
  | Arith.Zero_nat -> 0
  | Arith.Suc n -> 1 + int_of_nat n
;;

let nth_bit x n = x land (1 lsl n) <> 0 ;;

let char_of_char (c: char) : Str.char =
  let imp idx = nth_bit (int_of_char c) idx in
  Chara (imp 0, imp 1, imp 2, imp 3, imp 4, imp 5, imp 6, imp 7)
;;

let () =
  let input = read_lines "day_NN/input.txt"
    |> BaseList.map (fun l -> get_chars l |> List.map char_of_char)
  in
  DayNN.pt01 input |> int_of_nat |> Printf.printf "Pt01: %i\n" ;
  DayNN.pt02 input |> int_of_nat |> Printf.printf "Pt02: %i\n" ;
