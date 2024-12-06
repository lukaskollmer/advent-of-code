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


type direction = Up | Down | Left | Right

type tile = Free | Obstacle | Player of direction;;

type map = tile array array ;;


let tile_of_char = function
  | '.' -> Free
  | '#' -> Obstacle
  | '^' -> Player Up
  | 'v' -> Player Down
  | '<' -> Player Left
  | '>' -> Player Right
  | _ -> failwith "invalid tile"
;;


let parse input =
  let size = List.length input in
  let map = Array.init size (fun _ -> Array.make size Free) in
  input |> List.iteri (fun row_idx row ->
    row |> List.iteri (fun col_idx col ->
      map.(row_idx).(col_idx) <- tile_of_char col
    )
  ) ;
  map
;;



let copy map =
  let size = Array.length map in
  Array.init size (fun i -> Array.copy map.(i))
;;


let get_player_pos map =
  map |> Array.find_mapi (fun row_idx row ->
    match Array.find_mapi (fun col_idx -> function Player dir -> Some (col_idx, dir) | _ -> None) row with
      | None -> None
      | Some (col_idx, dir) -> Some (col_idx, row_idx, dir)
  )
;;

let next_pos_in_dir (x,y) = function
  | Up -> (x,y-1)
  | Down -> (x,y+1)
  | Left -> (x-1,y)
  | Right -> (x+1,y)
;;


let is_valid_pos x y map =
  let size = Array.length map in
  x >=0 && x < size && y >=0 && y < size
;;


let turn = function
  | Up -> Right
  | Right -> Down
  | Down -> Left
  | Left -> Up
;;


let step map =
  let map = copy map in
  match get_player_pos map with
    | None -> (false, map)
    | Some (x, y, dir) -> (
      let (x',y') = next_pos_in_dir (x,y) dir in
      if not (is_valid_pos x' y' map) then (
        (* player walked off map *)
        Printf.printf "step: [%i, %i] -> [%i, %i] player walked off map\n%!" x y x' y' ; 
        map.(y).(x) <- Free ; (true, map))
      else
        (match map.(y').(x') with
          | Free -> (* can walk there *)
            map.(y).(x) <- Free ;
            map.(y').(x') <- Player dir ;
            Printf.printf "step: [%i, %i] -> [%i, %i] normal walk\n%!" x y x' y' ;
            true, map
          | Obstacle -> (* need to turn *)
            Printf.printf "step: [%i, %i] -> [%i, %i] turn\n%!" x y x' y' ;
            map.(y).(x) <- Player (turn dir) ;
            (true, map)
          | Player _ -> failwith "invalid")
    )
;;


let run map =
  let rec imp acc map =
    match step map with
      | false, map -> List.rev acc
      | true, map -> imp (map::acc) map
  in
  imp [] map
;;


module PosSet = Set.Make(struct
  type t = int * int
  let compare = compare
end) ;;


let pt01 map =
  let steps = run map in
  let all_player_locs = steps |> List.fold_left (fun acc map ->
    match get_player_pos map with
      | None -> acc
      | Some (x,y,_) -> PosSet.add (x,y) acc
  ) PosSet.empty in
  PosSet.cardinal all_player_locs
;;




let () =
  let input = read_lines "input.txt" in
  let map = input |> List.map get_chars |> parse in
  map |> pt01 |> Printf.printf "pt01: %i\n%!" ;