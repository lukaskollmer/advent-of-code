#use "topfind" ;;
#require "extlib" ;;


let input = open_in "input.txt" |> Std.input_list

(* part 1 *)

let get_chars s =
  let rec imp l = function
    | -1 -> l
    | i  -> imp (s.[i] :: l) (i-1)
  in
  imp [] (String.length s - 1)

let count_of e l =
  let rec imp acc = function [] -> acc
    | x::xs ->
      if x = e then imp (acc+1) xs
      else imp acc xs
  in
  imp 0 l


let get_counts l = List.map (fun e -> count_of e l, e) l

let num_with_count count input = input
  |> List.map get_chars
  |> List.filter (fun chars -> chars |> get_counts |> List.exists (fun (c, _) -> c = count))
  |> List.length

let num_twice = num_with_count 2 input
let num_thrice = num_with_count 3 input

let _ = num_twice * num_thrice |> print_int