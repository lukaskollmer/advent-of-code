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

module PosSet = Set.Make(struct
  type t = int * int
  let compare = compare
end) ;;


module PosSet' = Set.Make(struct
  type t = int * int * direction
  let compare = compare
end) ;;



type state = Ok of (int*int*direction) | OffBounds | Loop ;;


let rec step seen map =
  let map = copy map in
  match get_player_pos map with
    | None -> (OffBounds, map)
    | Some (x, y, dir) -> (
      let (x',y') = next_pos_in_dir (x,y) dir in
      if not (is_valid_pos x' y' map) then (
        (* player walked off map *)
        (* Printf.printf "step: [%i, %i] -> [%i, %i] player walked off map\n%!" x y x' y' ;  *)
        map.(y).(x) <- Free ; (OffBounds, map))
      else
        (match map.(y').(x') with
          | Free -> (* can walk there *)
            map.(y).(x) <- Free ;
            map.(y').(x') <- Player dir ;
            (* Printf.printf "step: [%i, %i] -> [%i, %i] normal walk\n%!" x y x' y' ; *)
            (if PosSet'.mem (x',y', dir) seen then Loop else Ok (x',y',dir)), map
          | Obstacle -> (* need to turn *)
            (* Printf.printf "step: [%i, %i] -> [%i, %i] turn\n%!" x y x' y' ; *)
            map.(y).(x) <- Player (turn dir) ;
            step seen map
            (* Ok (x',y'), map *)
          | Player _ -> failwith "invalid")
    )
;;



let run map =
  let rec imp seen map =
    match step seen map with
      | OffBounds, _ -> OffBounds, seen
      | Ok pos, map -> imp (PosSet'.add pos seen) map
      | Loop, map -> Loop, seen
  in
  imp PosSet'.empty map
;;



let count_where p l =
  l |> List.fold_left (fun acc x -> acc + if p x then 1 else 0) 0
;;


let add_obstacle map x y =
  let map = copy map in
  map.(y).(x) <- Obstacle ;
  map
;;


let has_loop map =
  match run map with
    | Loop, _ -> true
    | _, _ -> false
;;


let drop_first n l =
  let rec imp = function
    | (0, xs) -> xs
    | (_, []) -> []
    | (n, x::xs) -> imp (n-1, xs)
  in imp (n, l)
;;

let take n l =
  let rec imp acc = function
    | (0, _) -> List.rev acc
    | (_, []) -> List.rev acc
    | (n, x::xs) -> imp (x::acc) (n-1, xs)
  in imp [] (n, l)
;;


let chunked n l =
  if n = 0 then [l] else
  let rec imp acc = function
    | [] -> List.rev acc
    | l -> imp ((take n l)::acc) (drop_first n l)
  in imp [] l
;;



let solve map =
  let (_, reachable) = run map in
  let reachable' = reachable |> PosSet'.to_list |> List.map (fun (x,y,_) -> (x,y)) |> PosSet.of_list in
  reachable' |> PosSet.cardinal |> Printf.printf "pt01: %i\n%!" ;
  let add_obstacle = add_obstacle map in
  Printf.printf "Running pt02\n%!" ;
  if false then (
    let idx = ref 0 in
    reachable' |> PosSet.to_list |> count_where (fun (x,y) ->
      Printf.printf "%i of %i\n%!" (!idx) (reachable' |> PosSet.cardinal) ;
      incr idx ;
      has_loop (add_obstacle x y)
    ) |> Printf.printf "pt02: %i\n%!" ;
  ) else if false then (
    reachable' |> PosSet.to_list |> List.mapi (fun i (x,y) ->
      Domain.spawn (fun () ->
        Printf.printf "run %i\n%!" i ;
        has_loop (add_obstacle x y)
      )
    ) |> List.map Domain.join |> List.map (Bool.to_int) |> List.fold_left (+) 0
    |> Printf.printf "pt02: %i\n%!" ;
  ) else (
    reachable'
    |> PosSet.to_list
    |> List.mapi (fun i (x,y) -> (i,x,y))
    |> chunked 750
    |> List.map (fun poss -> Domain.spawn (fun () -> poss|> List.fold_left (fun acc (i,x,y) -> 
      Printf.printf "run %i\n%!" i ; (add_obstacle x y) |> has_loop  |> Bool.to_int
      ) 0))
    |> List.map Domain.join |> List.fold_left (+) 0
    |> Printf.printf "pt02: %i\n%!" ;
  )
;;




let () =
  let input = read_lines "input.txt" in
  let map = input |> List.map get_chars |> parse in
  solve map ;