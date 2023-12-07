let read_lines filename =
  let file = open_in filename in
  let rec imp acc = 
    try imp (input_line file :: acc)
    with End_of_file -> close_in file; List.rev acc
  in
  imp []
;;



let count_where f l =
  List.fold_left (fun acc x -> if f x then (acc+1) else acc) 0 l
;;


(* let rec mk_range_to_excl = function
  | 0 -> []
  | n -> (mk_range_to_excl (n-1)) @ [n]
;; *)

let mk_range_to_excl n =
  let rec imp acc = function
    | 0 -> List.rev acc
    | n -> imp (n::acc) (n-1)
  in imp [] n
;;


type race = int * int



let calc_distances n =
  mk_range_to_excl n |> List.map (fun n' -> (n-n')*n')
;;



let pt01 input =
  let input = input
    |> List.map (String.split_on_char ' ')
    |> List.map (List.filter (fun l -> String.length l <> 0))
    |> List.map List.tl
    |> List.map (List.map int_of_string)
  in
  List.combine (List.nth input 0) (List.nth input 1)
    |> List.map (fun (t, r) -> calc_distances t |> count_where (fun d -> d > r))
    |> List.fold_left ( * ) 1
;;


let pt02 (input: string list) =
  let input = input
    |> List.map (String.split_on_char ' ')
    |> List.map (List.filter (fun l -> String.length l <> 0))
    |> List.map List.tl
    |> List.map (String.concat "")
    |> List.map int_of_string
  in
  let time, record = (List.nth input 0, List.nth input 1) in
  Printf.printf "time: %i, record: %i\n%!" time record ;
  calc_distances time |> count_where (fun d -> d > record)
;;





let () =
  let input = read_lines "input.txt" in
  input |> pt01 |> Printf.printf "Pt01: %i\n" ;
  input |> pt02 |> Printf.printf "Pt02: %i\n" ;
;;