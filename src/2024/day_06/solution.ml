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

let chunked' n l =
  let size = List.length l / n in
  Printf.printf "chunk_size: %i\n%!" size ;
  chunked size l
;;

let chunked'' l = chunked' (Domain.recommended_domain_count ()) l;;



type direction = Up | Down | Left | Right

type tile = Free | Obstacle ;;


module PosSet = Set.Make(struct
  type t = int * int
  let compare = compare
end) ;;

module PosSet' = Set.Make(struct
  type t = int * int * direction
  let compare = compare
end) ;;


let tile_of_char = function
  | '.' -> Free
  | '#' -> Obstacle
  | '^' | 'v' | '<' | '>' -> Free
  | _ -> failwith "invalid tile"
;;


let size = 130 ;;
(* let size = 10 ;; *)


let get map x y =
  let offset = y * (size) + x in
  map.(offset)
;;

let set map x y v =
  let offset = y * (size) + x in
  map.(offset) <- v
;;



let parse input =
  let size = List.length input in
  let map = Array.make (size*size) Free in
  let player = ref (0, 0, Up) in
  input |> List.iteri (fun y row ->
    row |> List.iteri (fun x col ->
      set map x y (tile_of_char col) ;
      match (match col with '^' -> Some Up | 'v' -> Some Down | '<' -> Some Left | '>' -> Some Right | _ -> None) with
        | None -> ()
        | Some dir -> player := (x, y, dir)
    )
  ) ;
  map, !player
;;


let next_pos_in_dir (x,y) = function
  | Up    -> (x,y-1)
  | Down  -> (x,y+1)
  | Left  -> (x-1,y)
  | Right -> (x+1,y)
;;


let turn = function
  | Up    -> Right
  | Right -> Down
  | Down  -> Left
  | Left  -> Up
;;



type state = Ok of (int*int*direction) | OffBounds | Loop ;;


let valid_pos x y =
  x >=0 && x < size && y >=0 && y < size
;;


let rec step seen get (x,y,dir) =
  if not (valid_pos x y) then OffBounds else
  let (x',y') = next_pos_in_dir (x,y) dir in
  if not (valid_pos x' y') then OffBounds else
  match get x' y' with
  | Free -> (* can walk there *)
    if PosSet'.mem (x',y', dir) seen then 
      Loop
    else
      Ok (x',y',dir)
  | Obstacle -> (* need to turn *)
    step seen get (x,y,turn dir)
;;



let run get initial_player_pos =
  let rec imp seen map player =
    match step seen get player with
      | OffBounds -> OffBounds, seen
      | Ok pos -> imp (PosSet'.add pos seen) get pos
      | Loop -> Loop, seen
  in
  imp (PosSet'.of_list [initial_player_pos]) get initial_player_pos
;;




let add_obstacle get x y =
  fun x' y' -> if x=x' && y=y' then
    Obstacle
  else
    get x' y'
;;



let solve get player_pos =
  (* Pt01 (and 02 prep) *)
  let (_, reachable) = run get player_pos in
  let reachable' = reachable |> PosSet'.to_list |> List.map (fun (x,y,_) -> (x,y)) |> PosSet.of_list in
  reachable'
  |> PosSet.cardinal
  |> Printf.printf "pt01: %i\n%!" ;
  let has_loop get =
    match run get player_pos with
      | Loop, _ -> true
      | _, _ -> false
  in
  (* Pt02 *)
  reachable'
  |> PosSet.to_list
  |> List.mapi (fun i (x,y) -> (i,x,y))
  |> chunked' 8
  |> List.map (fun poss -> Domain.spawn (fun () -> poss|> List.fold_left (fun acc (i,x,y) -> 
    acc + ((add_obstacle get x y) |> has_loop |> Bool.to_int)
    ) 0))
  |> List.map Domain.join |> List.fold_left (+) 0
  |> Printf.printf "pt02: %i\n%!" ;
;;



let () =
  let input = read_lines "input.txt" in
  let map, player = input |> List.map get_chars |> parse in
  solve (get map) player;
