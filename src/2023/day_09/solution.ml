let read_lines filename =
  let file = open_in filename in
  let rec imp acc = 
    try imp (input_line file :: acc)
    with End_of_file -> close_in file; List.rev acc
  in
  imp []
;;



let last l = List.nth l (List.length l - 1) ;;


let rec calc_diffs = function
  | [] -> []
  | [x1] -> []
  | x1::x2::xs -> (x2-x1)::calc_diffs (x2::xs)
;;



type direction = Fwd | Bckwd


let calc_next dir l =
  let rec imp l =
    if List.for_all (fun n -> n = 0) l then 0
    else
      let next = imp (calc_diffs l) in
      match dir with
        | Fwd ->  last l + next
        | Bckwd -> List.hd l - next
  in
  imp l
;;


let run calc_next input =
  input
    |> List.map (String.split_on_char ' ')
    |> List.map (List.map int_of_string)
    |> List.map calc_next
    |> List.fold_left (+) 0
;;

let pt01 = run (calc_next Fwd) ;;
let pt02 = run (calc_next Bckwd) ;;


let () =
  let input = read_lines "input.txt" in
  input |> pt01 |> Printf.printf "Pt01: %i\n" ;
  input |> pt02 |> Printf.printf "Pt01: %i\n" ;
