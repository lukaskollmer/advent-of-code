let read_lines filename =
  let file = open_in filename in
  let rec imp acc = 
    try imp (input_line file :: acc)
    with End_of_file -> close_in file; List.rev acc
  in
  imp []
;;


let get_opt = function Some x -> x | None -> failwith "None" ;;


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


let print_map map =
  map |> Array.iteri (fun y line ->
    line |> Array.iteri (fun x tile ->
      Printf.printf "%c" (tile_to_char tile);
    ) ;
    Printf.printf "\n" ;
  ) ;
;;


module TileMap = struct
  type tiles_array = tile array array
  type distances_array = int array array

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
    (* Printf.printf "find_start\n" ; *)
    let x = map |> Array.find_mapi (fun y line ->
      let x = line |> Array.find_index (fun tile -> tile = Start) in
      match x with Some x -> Some (x, y) | None -> None
    ) in
    match x with
      | Some x -> x
      | None -> failwith "no start?"
  ;;

  let valid_pos x y map =
    let w, h = dims map in
    x >= 0 && x < w && y >= 0 && y < h
  ;;

  let at x y map =
    assert (valid_pos x y map) ;
    map.(y).(x) ;;

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
    (* Printf.printf "START: (%i, %i)\n" x_start y_start ; *)
    (* let x, y, dir = [(1,0, Left); (0,1, Top); (-1,0, Right); (0,-1, Bottom)] |> List.filter_map (fun (x,y, d) ->
      let tile = at (x_start+x) (y_start+y) map in
      if pipe_connects_in_dir tile d then Some (x_start+x, y_start+y, d) else None
      (* match tile with
        | Ground -> None
        | Start -> failwith "DJEUWOVTEN GT"
        | TopAndBottom | LeftAndRight | TopAndLeft | TopAndRight | BottomAndLeft | BottomAndRight -> Some (x_start+x, y_start+y, d) *)
    ) |> List.hd in
    step x y 1 dir [(x_start, y_start, dir)]; *)
    let paths = [(1,0, Left); (0,1, Top); (-1,0, Right); (0,-1, Bottom)]
      |> List.map (fun (x, y, dir) -> (x_start+x, y_start+y, dir))
      |> List.filter (fun (x, y, dir) -> valid_pos x y map && pipe_connects_in_dir (at x y map) dir)
      |> List.map (fun (x, y, dir) ->
      step x y 1 dir [(x_start, y_start, dir)];
    ) in
    (* paths |> List.iter (fun path ->
      Printf.printf "\n\n\nPATH:\n" ;
      List.iter (fun (x, y, src) -> Printf.printf "(%i,%i,%s) " x y (string_of_src_dir src)) path;
    ) ; *)
    (* Printf.printf "\n" ; *)
    let path = paths |> List.hd |> List.map (fun (x, y, _) -> (x, y)) in
    (path, distances)
  ;;


  let is_border x y map =
    let w, h = dims map in
    (* Printf.printf "border dims: %i, %i\n" w h ; *)
    (* Printf.printf "is_border (%i, %i): x=0: %b, y=0: %b, x=w-1: %b, y=h-1: %b\n" x y (x = 0) (y = 0) (x = w-1) (y = h-1) ; *)
    x = 0 || y = 0 || x = w-1 || y = h-1
  ;;

  let enlargen map =
    let w, h = dims map in
    let map =
    Array.init (2*h+1) (fun y -> Array.init (2*w+1) (fun x ->
      if (y mod 2 = 0) || (x mod 2 = 0) then Ground else map.(y / 2).(x / 2)
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
    (* Printf.printf "\n\nMAP PRE ENLARGEN FIX:\n" ;
    print_map map ; *)
    let w, h = dims map in
    for y = 0 to h-1 do
      for x = 0 to w-1 do
        (* if (x-1) > 0 && (x+1) < w && at x y map = Ground && pipe_connects_in_dir (at (x-1) y map) Right && pipe_connects_in_dir (at (x+1) y map) Left then
          map.(y).(x) <- LeftAndRight ; *)
          imp x y (x-1) y (x+1) y Right Left LeftAndRight ;
          imp x y (x) (y-1) x (y+1) Bottom Top TopAndBottom ;
      done
    done ;
    (* Printf.printf "\n\nMAP POST ENLARGEN FIX:\n" ;
    print_map map ; *)
    map
    ;;

  type tile_type = Pipe of tile | Inside | Outside
  let reachability map =
    let path, _ = find_loop map in
    let map' = map |> Array.mapi (fun y -> Array.mapi (fun x t ->
      if not (List.mem (x,y) path) then (Inside, false) else
      match t with
        | Ground -> ((if is_border x y map then Outside else Inside), false)
        | t -> (Pipe t, true)
    )) in
    let rec imp x y path =
      (* if (not (valid_pos x y map)) || (snd (at x y map') = true) then () *)
      if (not (valid_pos x y map)) || (path |> List.tl |> List.mem (x,y)) then ()
      else begin
        (* Printf.printf "[imp] exploring (%i, %i)\n" x y ; *)
        (* TODO *)
        [(1,0); (0,1); (-1,0); (0,-1)]
          |> List.iter (fun (x', y') ->
            if valid_pos (x+x') (y+y') map' then
              match at (x+x') (y+y') map' with
                | (Pipe _, _) -> ()
                | (Outside, true) -> ()
                | (_, false) -> begin
                    map'.(y+y').(x+x') <- (Outside, true) ;
                    imp (x+x') (y+y') ((x+x',y+y')::path) ;
                  end
                | (Inside, true) -> failwith "dafuq"
            (* else Printf.printf "skipping (%i, %i) bc its not a valid pos\n" (x+x') (y+y') ; *)
          )
      end
    in
    let w, h = dims map in
    for y = 0 to h-1 do
      for x = 0 to w-1 do
        if is_border x y map then imp x y [(x,y)]
        (* else Printf.printf "skipping (%i, %i) bc not a border\n" x y ; *)
      done
    done ;
    (* map' |> Array.iteri (fun y l ->
      if y mod 2 = 0 then () else begin
        Printf.printf "%3d | " (y/2) ;
        Array.iteri (fun x (t,_) ->
          if x mod 2 = 1 then
          Printf.printf "%s" (match t with Pipe t -> (String.of_seq (List.to_seq [tile_to_char t])) | Inside -> "~" | Outside -> "O")
        ) l ;
        Printf.printf "\n" ;
      end
    ) ; *)
    (* Array.fold_left (fun acc l -> acc + Array.fold_left (fun acc (t,_) -> acc + if t = Inside then 1 else 0) 0 l) 0 map' *)
    map'
      |> Array.mapi (fun y l -> (y, l |> Array.mapi (fun x (t,_) -> (x, t))))
      |> Array.fold_left (fun acc (y, l) -> acc + (l |> Array.fold_left (fun acc (x, t) -> acc + if y mod 2 = 1 && x mod 2 = 1 && t = Inside then 1 else 0) 0)) 0
  ;;
end


let pt01 map =
  let _, distances = TileMap.find_loop map in
  (* distances |> Array.iteri (fun y line ->
    line |> Array.iteri (fun x distance ->
      Printf.printf "%s " (if distance <> Int.max_int then (Printf.sprintf "%3d" distance) else " - ") ;
    ) ;
    Printf.printf "\n" ;
  ) ; *)

  (* Printf.printf "DISTANCES ARRAY"
  distances |> Array.iteri (fun y l ->
    if y mod 2 = 0 then () else begin
      Printf.printf "%3d | " (y/2) ;
      Array.iteri (fun x d ->
        if x mod 2 = 1 then
        Printf.printf "%s" (if d = Int.max_int then "" else string_of_int d)
      ) l ;
      Printf.printf "\n" ;
    end
  ) ; *)

  (* distances |> Array.iteri (fun y line ->
    line |> Array.iteri (fun x distance ->
      Printf.printf "%s " (if distance <> Int.max_int then "X" else ".") ;
    ) ;
    Printf.printf "\n" ;
  ) ; *)

  distances |> Array.fold_left (fun acc line ->
    let line_max = line |> Array.fold_left (fun acc d -> if d <> Int.max_int then (max acc d) else acc) 0 in
    max acc line_max
  ) 0
;;


let () =
  let input = read_lines "input.txt" in
  let map = input |> TileMap.make in
  (* Printf.printf "H: %i, W: %i\n" (List.length input) (input |> List.map (String.length) |> List.fold_left (max) 0) ; *)
  (* print_map map ; *)
  map |> pt01 |> Printf.printf "Pt01: %i\n" ;
  (* Printf.printf "\n\n\n\n\n\n\n" ; *)
  let map = TileMap.enlargen map in
  (* print_map map ; *)
  map |> TileMap.reachability |> Printf.printf "Pt02: %i\n" ;
