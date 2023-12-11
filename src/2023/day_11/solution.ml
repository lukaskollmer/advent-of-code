module IntSet = Set.Make(Int) ;;

module PosPairSet = Set.Make(struct
  type t = (int * int) * (int * int)
  let compare = compare
end) ;;


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
    | i  -> imp (s.[i] :: l) (i-1)
  in
  imp [] (String.length s - 1)
;;

let flat_mapi f l =
  let rec imp i acc = function
    | [] -> acc
    | x::xs -> imp (i+1) (acc @ f i x) xs
  in imp 0 [] l
;;

let set_of_range a b =
  let rec imp acc = function
    | x when x = b -> acc
    | x -> imp (IntSet.add x acc) (x+1)
  in
  imp IntSet.empty a
;;


(* Parse input; return list of galaxies, universe width, universe height *)
let parse_input input =
  let univ = input |> flat_mapi (fun y l -> l |> get_chars |> flat_mapi (fun x -> function '#' -> [(x,y)] | _ -> [])) in
  (univ, input |> List.hd |> String.length, List.length input)
;;


let enlargen n w h univ =
  let n = max 1 (n-1) in
  let empty_rows, empty_cols = univ |> List.fold_left (fun (rows, cols) (x, y) ->
    (IntSet.remove y rows, IntSet.remove x cols)
  ) (set_of_range 0 h, set_of_range 0 w) in
  let count_lt x s = s |> IntSet.filter ((>=) x) |> IntSet.cardinal in
  univ |> List.map (fun (x,y) -> (
    x + n * (count_lt x empty_cols),
    y + n * (count_lt y empty_rows)
  ))
;;


let run factor input =
  let univ, w, h = parse_input input in
  let univ = univ |> enlargen factor w h in
  let pairs = univ |> List.fold_left (fun pairs g1 ->
    univ |> List.fold_left (fun pairs g2 ->
      if g1 = g2 || PosPairSet.mem (g1,g2) pairs || PosPairSet.mem (g2,g1) pairs
      then pairs
      else PosPairSet.add (g1,g2) pairs
    ) pairs
  ) PosPairSet.empty in
  pairs
    |> PosPairSet.to_list
    |> List.map (fun ((x,y), (x',y')) -> abs (x-x') + abs (y-y'))
    |> List.fold_left (+) 0
;;


let pt01 = run 1 ;;
let pt02 = run 1_000_000 ;;


let () =
  let input = read_lines "input.txt" in
  input |> pt01 |> Printf.printf "Pt01: %d\n" ;
  input |> pt02 |> Printf.printf "Pt02: %d\n" ;
