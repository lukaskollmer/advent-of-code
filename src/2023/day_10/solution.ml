let read_lines filename =
  let file = open_in filename in
  let rec imp acc = 
    try imp (input_line file :: acc)
    with End_of_file -> close_in file; List.rev acc
  in
  imp []
;;


let get_opt = function Some x -> x | None -> failwith "None" ;;

let even n = n mod 2 == 0 ;;
let odd n = not (even n) ;;


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


type direction = Top | Bottom | Left | Right ;;

let string_of_direction = function
  | Top -> "Top"
  | Bottom -> "Bottom"
  | Left -> "Left"
  | Right -> "Right"
;;



module TileMap = struct
  let make input =
    let h = List.length input in
    let w = input |> List.map (String.length) |> List.fold_left max 0 in
    Array.init h (fun y ->
      let line = List.nth input y in
      Array.init w (fun x ->
        String.get line x |> parse_tile
      )
    )
  ;;

  let dims map =
    (Array.length map.(0), Array.length map)
  ;;

  let find_start map =
    map |> Array.find_mapi (fun y line ->
      let x = line |> Array.find_index (fun tile -> tile = Start) in
      match x with Some x -> Some (x, y) | None -> None
    ) |> get_opt
  ;;

  let valid_pos x y map =
    let w, h = dims map in
    x >= 0 && x < w && y >= 0 && y < h
  ;;

  let at x y map =
    map.(y).(x)
  ;;

  let set x y v map = map.(y).(x) <- v ;;

  let pipe_connects_in_dir = function
    | TopAndBottom -> (function Top | Bottom -> true | _ -> false)
    | LeftAndRight -> (function Left | Right -> true | _ -> false)
    | TopAndLeft -> (function Top | Left -> true | _ -> false)
    | TopAndRight -> (function Top | Right -> true | _ -> false)
    | BottomAndLeft -> (function Bottom | Left -> true | _ -> false)
    | BottomAndRight -> (function Bottom | Right -> true | _ -> false)
    | Start -> fun _ -> true
    | Ground -> fun _ -> false
  ;;


  let find_loop map =
    let w, h = dims map in
    let distances = Array.make_matrix h w (Int.max_int) in
    let rec step x y d src path =
      let path = path @ [(x,y,src)] in
      if not (valid_pos x y map) then path
      else match at x y map with
        | Ground -> failwith "unreachable?"
        | Start -> path
        | TopAndBottom -> begin
          if at x y distances > d then distances.(y).(x) <- d ;
          match src with
            | Top -> step x (y+1) (d+1) Top path
            | Bottom -> step x (y-1) (d+1) Bottom path
            | _ -> failwith (Printf.sprintf "[A] unexpected src: '%s'" (string_of_direction src))
          end
        | LeftAndRight -> begin
          if at x y distances > d then distances.(y).(x) <- d ;
          match src with
            | Left -> step (x+1) y (d+1) Left path
            | Right -> step (x-1) y (d+1) Right path
            | _ -> failwith (Printf.sprintf "[B] unexpected src: '%s'" (string_of_direction src))
          end
        | TopAndLeft -> begin
          if at x y distances > d then distances.(y).(x) <- d ;
          match src with
            | Top -> step (x-1) y (d+1) Right path
            | Left -> step x (y-1) (d+1) Bottom path
            | _ -> failwith (Printf.sprintf "[C] unexpected src: '%s'" (string_of_direction src))
          end
        | TopAndRight -> begin
          if at x y distances > d then distances.(y).(x) <- d ;
          match src with
            | Top -> step (x+1) y (d+1) Left path
            | Right -> step x (y-1) (d+1) Bottom path
            | _ -> failwith (Printf.sprintf "[D] unexpected src: '%s'" (string_of_direction src))
          end
        | BottomAndLeft -> begin
          if at x y distances > d then distances.(y).(x) <- d ;
          match src with
            | Bottom -> step (x-1) y (d+1) Right path
            | Left -> step x (y+1) (d+1) Top path
            | _ -> failwith (Printf.sprintf "[E] unexpected src: '%s'" (string_of_direction src))
          end
        | BottomAndRight -> begin
          if at x y distances > d then distances.(y).(x) <- d ;
          match src with
            | Bottom -> step (x+1) y (d+1) Left path
            | Right -> step x (y+1) (d+1) Top path
            | _ -> failwith (Printf.sprintf "[F] unexpected src: '%s'" (string_of_direction src))
          end
    in
    let x_start, y_start = find_start map in
    let paths = [(1,0, Left); (0,1, Top); (-1,0, Right); (0,-1, Bottom)]
      |> List.map (fun (x, y, dir) -> (x_start+x, y_start+y, dir))
      |> List.filter (fun (x, y, dir) -> valid_pos x y map && pipe_connects_in_dir (at x y map) dir)
      |> List.map (fun (x, y, dir) ->
      step x y 1 dir [(x_start, y_start, dir)];
    ) in
    (* since there is only one loop, all paths will be along the same tiles,
       though maybe in opposite directions; we can safely take the head. *)
    let path = paths |> List.hd |> List.map (fun (x, y, _) -> (x, y)) in
    (path, distances)
  ;;


  let is_border x y map =
    let w, h = dims map in
    x = 0 || y = 0 || x = w-1 || y = h-1
  ;;

  let is_real_coord x y = odd x && odd y ;;

  (* "Enlargens" the map, by inserting extra rows and colums between all rows and columns of the input map.
     If the input map has dimension w*h, the output has dimension (2w+1)*(2h+1).
     The added cols/rows are filled with Ground tiles.
     The reason we do this is in order to be able to deal with the  *)
  let enlargen map =
    let w, h = dims map in
    let map =
    Array.init (2*h+1) (fun y -> Array.init (2*w+1) (fun x ->
      if even y || even x then Ground else map.(y / 2).(x / 2)
    )) in
    let imp x y x_pre y_pre x_post y_post dir_pre dir_post t =
      if valid_pos x_pre y_pre map
        && valid_pos x_post y_post map
        && at x y map = Ground
        && pipe_connects_in_dir (at x_pre y_pre map) dir_pre
        && pipe_connects_in_dir (at x_post y_post map) dir_post
      then
        map.(y).(x) <- t ;
    in
    let w, h = dims map in
    for y = 0 to h-1 do
      for x = 0 to w-1 do
        (* Insert connecting pipe segments between matching pipes *)
        imp x y (x-1) y (x+1) y Right Left LeftAndRight ;
        imp x y (x) (y-1) x (y+1) Bottom Top TopAndBottom ;
      done
    done ;
    map
  ;;

  type tile_type = Pipe of tile | Inside | Outside ;;

  let reachability map =
    let path, _ = find_loop map in
    let map' = map |> Array.mapi (fun y -> Array.mapi (fun x t ->
      if not (List.mem (x,y) path) then Inside else
      match t with
        | Ground -> if is_border x y map then Outside else Inside
        | t -> Pipe t
    )) in
    let rec imp x y path =
      if (not (valid_pos x y map)) || (path |> List.tl |> List.mem (x,y)) then ()
      else begin
        [(1,0); (0,1); (-1,0); (0,-1)]
          |> List.map (fun (x', y') -> (x+x', y+y'))
          |> List.filter (fun (x, y) -> valid_pos x y map')
          |> List.iter (fun (x, y) ->
            match at x y map' with
              | Pipe _ -> ()
              | Outside -> ()
              | _ ->
                  map'.(y).(x) <- Outside ;
                  imp x y ((x,y)::path) ;
          )
      end
    in
    let w, h = dims map in
    for y = 0 to h-1 do
      for x = 0 to w-1 do
        if is_border x y map then imp x y [(x,y)]
      done
    done;
    map'
      |> Array.mapi (fun y l -> (y, l |> Array.mapi (fun x t -> (x, t))))
      |> Array.fold_left (fun acc (y, l) ->
        acc + (l |> Array.fold_left (fun acc (x, t) ->
          acc + if is_real_coord x y && t = Inside then 1 else 0
        ) 0)
      ) 0
  ;;
end


let pt01 map =
  let _, distances = TileMap.find_loop map in
  distances |> Array.fold_left (fun acc line ->
    let line_max = line |> Array.fold_left (fun acc d -> if d <> Int.max_int then (max acc d) else acc) 0 in
    max acc line_max
  ) 0
;;


let () =
  let input = read_lines "input.txt" in
  let map = input |> TileMap.make in
  map |> pt01 |> Printf.printf "Pt01: %i\n" ;
  map |> TileMap.enlargen |> TileMap.reachability |> Printf.printf "Pt02: %i\n" ;
