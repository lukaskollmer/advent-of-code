let read_lines filename =
  let file = open_in filename in
  let rec imp acc = 
    try imp (input_line file :: acc)
    with End_of_file -> close_in file; List.rev acc
  in
  imp []
;;


let get_opt = function Some x -> x | None -> failwith "None" ;;


(* type tile = NS | EW | NE | NW | SW | SE *)
type tile =
  | Ground | Start
  | TopAndBottom | LeftAndRight
  | TopAndLeft | TopAndRight
  | BottomAndLeft | BottomAndRight
;;

let parse_tile = function
  | '|' -> TopAndBottom
  | '-' -> LeftAndRight
  | 'L' -> TopAndRight
  | 'J' -> TopAndLeft
  | '7' -> BottomAndLeft
  | 'F' -> BottomAndRight
  | '.' -> Ground
  | 'S' -> Start
  | c -> failwith (Printf.sprintf "Unknown tile segment: '%c'" c)
;;

let tile_to_char = function
  | TopAndBottom -> '|'
  | LeftAndRight -> '-'
  | TopAndRight -> 'L'
  | TopAndLeft -> 'J'
  | BottomAndLeft -> '7'
  | BottomAndRight -> 'F'
  | Ground -> '.'
  | Start -> 'S'
;;


type src_dir = Top | Bottom | Left | Right ;;

let string_of_src_dir = function
  | Top -> "Top"
  | Bottom -> "Bottom"
  | Left -> "Left"
  | Right -> "Right"
;;


module TileMap = struct
  type tiles_array = tile array array
  type distances_array = int array array

  let make input =
    let len = List.length input in
    Array.init len (fun y ->
      let line = List.nth input y in
      Array.init len (fun x ->
        String.get line x |> parse_tile
      )
    )
  ;;

  let find_start map =
    map |> Array.find_mapi (fun y line ->
      let x = line |> Array.find_index (fun tile -> tile = Start) in
      match x with Some x -> Some (x, y) | None -> None
    ) |> get_opt
  ;;

  let at x y map = map.(y).(x) ;;

  let dump_dbg_path path =
    Printf.printf "PATH:\n" ;
    path |> List.iter (fun (x, y, src) -> Printf.printf "(%i, %i, from: %s)\n" x y (string_of_src_dir src))
  ;;

  let pipe_connects_in_dir = function
    | TopAndBottom -> (function Top | Bottom -> true | _ -> false)
    | LeftAndRight -> (function Left | Right -> true | _ -> false)
    | TopAndLeft -> (function Top | Left -> true | _ -> false)
    | TopAndRight -> (function Top | Right -> true | _ -> false)
    | BottomAndLeft -> (function Bottom | Left -> true | _ -> false)
    | BottomAndRight -> (function Bottom | Right -> true | _ -> false)
    | Start | Ground -> fun _ -> false
  ;;

  let valid_pos x y map =
    let len = Array.length map in
    x >= 0 && x < len && y >= 0 && y < len
  ;;


  let find_loop map =
    let len = Array.length map in
    let distances = Array.make_matrix len len (Int.max_int) in
    let rec step x y d src path =
      let path = path @ [(x,y,src)] in
      if x >= len || y >= len then ()
      else match at x y map with
        | Ground -> failwith "unreachable?"
        | Start -> ()
        | TopAndBottom -> begin
          if at x y distances > d then distances.(y).(x) <- d ;
          match src with
            | Top -> step x (y+1) (d+1) Top path
            | Bottom -> step x (y-1) (d+1) Bottom path
            | _ -> dump_dbg_path path ; failwith (Printf.sprintf "[A] unexpected src: '%s'" (string_of_src_dir src))
          end
        | LeftAndRight -> begin
          if at x y distances > d then distances.(y).(x) <- d ;
          match src with
            | Left -> step (x+1) y (d+1) Left path
            | Right -> step (x-1) y (d+1) Right path
            | _ -> dump_dbg_path path ; failwith (Printf.sprintf "[B] unexpected src: '%s'" (string_of_src_dir src))
          end
        | TopAndLeft -> begin
          if at x y distances > d then distances.(y).(x) <- d ;
          match src with
            | Top -> step (x-1) y (d+1) Right path
            | Left -> step x (y-1) (d+1) Bottom path
            | _ -> dump_dbg_path path ; failwith (Printf.sprintf "[C] unexpected src: '%s'" (string_of_src_dir src))
          end
        | TopAndRight -> begin
          if at x y distances > d then distances.(y).(x) <- d ;
          match src with
            | Top -> step (x+1) y (d+1) Left path
            | Right -> step x (y-1) (d+1) Bottom path
            | _ -> dump_dbg_path path ; failwith (Printf.sprintf "[D] unexpected src: '%s'" (string_of_src_dir src))
          end
        | BottomAndLeft -> begin
          if at x y distances > d then distances.(y).(x) <- d ;
          match src with
            | Bottom -> step (x-1) y (d+1) Right path
            | Left -> step x (y+1) (d+1) Top path
            | _ -> dump_dbg_path path ; failwith (Printf.sprintf "[E] unexpected src: '%s'" (string_of_src_dir src))
          end
        | BottomAndRight -> begin
          if at x y distances > d then distances.(y).(x) <- d ;
          match src with
            | Bottom -> step (x+1) y (d+1) Left path
            | Right -> step x (y+1) (d+1) Top path
            | _ -> dump_dbg_path path ; failwith (Printf.sprintf "[F] unexpected src: '%s'" (string_of_src_dir src))
          end
    in
    let x_start, y_start = find_start map in
    Printf.printf "START: (%i, %i)\n" x_start y_start ;
    (* let x, y, dir = [(1,0, Left); (0,1, Top); (-1,0, Right); (0,-1, Bottom)] |> List.filter_map (fun (x,y, d) ->
      let tile = at (x_start+x) (y_start+y) map in
      if pipe_connects_in_dir tile d then Some (x_start+x, y_start+y, d) else None
      (* match tile with
        | Ground -> None
        | Start -> failwith "DJEUWOVTEN GT"
        | TopAndBottom | LeftAndRight | TopAndLeft | TopAndRight | BottomAndLeft | BottomAndRight -> Some (x_start+x, y_start+y, d) *)
    ) |> List.hd in
    step x y 1 dir [(x_start, y_start, dir)]; *)
    [(1,0, Left); (0,1, Top); (-1,0, Right); (0,-1, Bottom)]
      |> List.map (fun (x, y, dir) -> (x_start+x, y_start+y, dir))
      |> List.filter (fun (x, y, dir) -> valid_pos x y map && pipe_connects_in_dir (at x y map) dir)
      |> List.iter (fun (x, y, dir) ->
      step x y 1 dir [(x_start, y_start, dir)];
    ) ;
    distances
  ;;
end


let pt01 map =
  let distances = TileMap.find_loop map in
  distances |> Array.iteri (fun y line ->
    line |> Array.iteri (fun x distance ->
      Printf.printf "%s " (if distance <> Int.max_int then (string_of_int distance) else "-") ;
    ) ;
    Printf.printf "\n" ;
  ) ;
  distances |> Array.fold_left (fun acc line ->
    let line_max = line |> Array.fold_left (fun acc d -> if d <> Int.max_int then (max acc d) else acc) 0 in
    max acc line_max
  ) 0
;;


let () =
  let input = read_lines "input.txt" in
  let map = TileMap.make input in
  Printf.printf "H: %i, W: %i\n" (List.length input) (input |> List.map (String.length) |> List.fold_left (max) 0) ;
  map |> Array.iteri (fun y line ->
    line |> Array.iteri (fun x tile ->
      Printf.printf "%c" (tile_to_char tile);
    ) ;
    Printf.printf "\n" ;
  ) ;
  map |> pt01 |> Printf.printf "Pt01: %i\n" ;
