let read_lines filename =
  let file = open_in filename in
  let rec imp acc = 
    try imp (input_line file :: acc)
    with End_of_file -> close_in file; List.rev acc
  in
  imp []
;;



let take n l =
  let rec imp acc = function
    | (0, _) -> List.rev acc
    | (_, []) -> List.rev acc
    | (n, x::xs) -> imp (x::acc) (n-1, xs)
  in imp [] (n, l)
;;


let drop_first n l =
  let rec imp = function
    | (0, xs) -> xs
    | (_, []) -> []
    | (n, x::xs) -> imp (n-1, xs)
  in imp (n, l)
;;


type tile = Empty | Box | Wall ;;
type inst = Up | Down | Left | Right ;;

let inst_of_char = function
  | '^' -> Up
  | 'v' -> Down
  | '<' -> Left
  | '>' -> Right
  | _ -> failwith ""
;;


let parse lines =
  let size = lines |> List.hd |> String.length in
  let map = Array.make_matrix size size Empty in
  let bot = ref None in
  lines |> take size |> List.iteri (fun y line -> line |> String.iteri (fun x c ->
    map.(y).(x) <- match c with
      | '.' -> Empty
      | '@' -> bot := Some (x,y); Empty
      | 'O' -> Box
      | '#' -> Wall
      | _ -> failwith "" ;
  )) ;
  (* TODO make this more efficient! (acc!!!) *)
  let insts = lines |> drop_first (size+1) |> List.fold_left (fun acc line ->
    line |> String.fold_left (fun acc c -> acc @ [inst_of_char c]) acc
  ) [] in
  map, Option.get !bot, insts
;;




let dump map bot insts =
  map |> Array.iteri (fun y row ->
    row |> Array.iteri (fun x tile ->
      if (x,y) = bot then
        print_char '@'
      else
        print_char (match tile with Empty -> '.' | Box -> 'O' | Wall -> '#')
    ) ;
    print_newline () ;
  ) ;
  print_newline () ;
  insts |> List.iter (fun inst ->
    print_char (match inst with Up -> '^' | Down -> 'v' | Left -> '<' | Right -> '>') ;
  ) ;
  print_newline () ;
;;



let next_pos (x,y) = function
  | Up -> x, y-1
  | Down -> x, y+1
  | Left -> x-1, y
  | Right -> x+1, y
;;


(* returns: true if we could move the box and the tile is now Empty *)
let rec push_box_in_dir map (x,y) dir =
  assert (map.(y).(x) = Box) ;
  let x',y' = next_pos (x,y) dir in
  match map.(y').(x') with
    | Empty ->
      (* the next tile is empty, so we can move the box there *)
      map.(y').(x') <- Box ;
      map.(y).(x) <- Empty ;
      true
    | Wall ->
      (* the next tile is a wall, so we can not move the box there. *)
      (* in fact, we cannot do anything. *)
      false
    | Box ->
      (* the next tile is also a box, so we try to move that *)
      if push_box_in_dir map (x',y') dir then
        (* if we were able to move the box on the next tile (x',y'),
          we try again to move the box at the current tile *)
        push_box_in_dir map (x,y) dir
      else
        false
;;


let rec step map (x,y as bot) = function
  | [] -> map, bot
  | inst::insts -> (
    assert (map.(y).(x) = Empty) ;
    let x',y' = next_pos bot inst in
    match map.(y').(x') with
      | Wall -> step map bot insts
      | Empty -> step map (x',y') insts
      | Box -> (
        if push_box_in_dir map (x',y') inst then
          step map (x',y') insts
        else
          step map bot insts
      )
  )
;;



let gps x y = x + 100 * y ;;


let pt01 map bot insts =
  (* dump map bot insts ; *)
  let map, bot = step map bot insts in
  dump map bot insts ;
  map |> Array.fold_left (fun (y, acc) row ->
    let _, acc = row |> Array.fold_left (fun (x, acc) tile ->
      let acc = match tile with
        | Empty | Wall -> acc
        | Box -> acc + gps x y
      in
      (x+1, acc)
    ) (0, acc)
    in
    (y+1, acc)
  ) (0,0) |> snd
;;


let () =
  let map, bot, insts = "input.txt" |> read_lines |> parse in
  pt01 map bot insts |> Printf.printf "pt01: %i\n%!" ;
  ()