#use "topfind" ;;
#require "extlib" ;;

let read_file filename =
  let chan = open_in filename in
  Std.input_list chan


let inputs = read_file "input.txt" |> List.map int_of_string

(* part 1 *)
let _ = inputs |> List.fold_left (+) 0 |> Printf.sprintf "part 1: %i" |> print_endline


(* part 2 *)
(* for some reason, this is _really_ slow (see, the js implementation) *)
let rec part2 freq freq_history = function
  | [] -> part2 freq freq_history inputs
  | v::inputs ->
    let freq = freq + v in
    if List.mem freq freq_history then
      freq
    else
      part2 freq (freq::freq_history) inputs

let _ = part2 0 [] inputs |> Printf.sprintf "part 2: %i" |> print_endline