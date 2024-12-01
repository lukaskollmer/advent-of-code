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



let take n l =
  let rec imp acc = function
    | (0, _) -> List.rev acc
    | (_, []) -> List.rev acc
    | (n, x::xs) -> imp (x::acc) (n-1, xs)
  in imp [] (n, l)
;;

let drop n l =
  let rec imp = function
    | (0, xs) -> xs
    | (_, []) -> []
    | (n, x::xs) -> imp (n-1, xs)
  in imp (n, l)
;;

let drop_end n l = take (List.length l - n) l ;;


let str_drop n s =
  if n < 0 then s
  else if n > String.length s then ""
  else String.sub s n (String.length s - n)
;;

let str_drop_end n s =
  if n < 0 then s
  else if n > String.length s then ""
  else String.sub s 0 (String.length s - n)
;;

type dir = Up | Down | Left | Right ;;
type step = dir * int ;;


let dir_of_char = function | 'U' -> Up | 'D' -> Down | 'L' -> Left | 'R' -> Right | _ -> failwith "invalid direction" ;;

let parse_step s =
  let split = String.split_on_char ' ' s in
  let dir = dir_of_char s.[0] in
  let len = List.nth split 1 |> int_of_string in
  let len', dir' = List.nth split 2 |> (fun s ->
    (* Printf.printf "%s\n%!" ((Printf.sprintf "0x%s" (s |> str_drop 2 |> str_drop_end 2))) ; *)
    let len' = int_of_string (Printf.sprintf "0x%s" (s |> str_drop 2 |> str_drop_end 2)) in
    let dir' = match s.[String.length s - 2] with
      | '0' -> Right
      | '1' -> Down
      | '2' -> Left
      | '3' -> Up
      | _ -> failwith "invalid input"
    in
    (len', dir')
  ) in
  (dir, len, dir', len')
;;




let advance_pos n (x,y) = function
  | Up -> (x, y-n)
  | Down -> (x, y+n)
  | Left -> (x-n, y)
  | Right -> (x+n, y)
;;

let advance_pos1 = advance_pos 1 ;;




let flat_map f l =
  let rec imp acc = function
    | [] -> acc
    | x::xs -> imp (f x @ acc) xs
  in imp [] l
;;



let _mk_path_acc_step_aux acc dir len =
  (* let cur = List.nth acc (List.length acc - 1) in *)
  let len, fix_i = if len >= 0 then len, (~+) else abs len, (~-) in
  List.rev_append (List.init len (fun i -> advance_pos (fix_i (i+1)) (List.hd acc) dir)) acc


let get_path start steps =
  steps |> List.fold_left (fun steps (dir, len) ->
    _mk_path_acc_step_aux steps dir len
  ) [start]
;;


let normalise_steps steps =
  let start = (0, 0) in
  let path, min_x, min_y, max_x, max_y, _ = steps |> List.fold_left (fun (path, min_x, min_y, max_x, max_y, cur) (dir, len) ->
    let (x, y) = advance_pos len cur dir in
    (* let path = List.rev_append (List.init (abs len) (fun i -> advance_pos ((if len >= 0 then (~+) else (~-))(i+1)) (List.hd path) dir)) path in *)
    let path = _mk_path_acc_step_aux path dir len in
    (path, min min_x x, min min_y y, max max_x x, max max_y y, (x,y))
  ) ([start], 0, 0, 0, 0, start) in
  Printf.printf "path:hd %d, %d\n%!" (fst (List.hd path)) (snd (List.hd path)) ;
  (* let path = List.rev path in *)
  Printf.printf "#path: %d; min_x: %d; min_y: %d; max_x: %d; max_y: %d\n%!" (List.length path) min_x min_y max_x max_y ;
  (* Printf.printf "path:\n%!" ;
  path |> List.iter (fun (x,y) -> Printf.printf "%d %d\n%!" x y) ; *)
  let x_adj = -min_x in
  let y_adj = -min_y in
  let path = path |> List.map (fun (x,y) -> (x+x_adj, y+y_adj)) in
  (* let steps = steps |> List.map (fun (dir, len, col) -> (dir, len + (
    match dir with
      | Up | Down -> y_adj
      | Left | Right -> x_adj
  ), col)) in *)
  let start = (
    let x, y = start in
    (x + x_adj, y + y_adj)
  ) in
  Printf.printf "new start: %d, %d\n%!" (fst start) (snd start) ;
  assert (List.hd path = start) ;
  begin
    let a = path in
    let b = get_path start steps in
    Printf.printf "paths: #a: %d; #b: %d\n%!" (List.length a) (List.length b) ;
    (* assert (List.length a = List.length b) ; *)
    List.iter2 (fun (x1,y1) (x2,y2) -> Printf.printf "a: (%d, %d); b: (%d, %d)\n%!" x1 y1 x2 y2 ; assert (x1 = x2 && y1 = y2)) a b ;
    assert (get_path start steps = path);
  end ;
  (* failwith "TODO"; *)
  let w, h = (
    (if min_x >= 0 then max_x + 1 else max_x - min_x + 1),
    (if min_y >= 0 then max_y + 1 else max_y - min_y + 1)
  ) in
  (* start, steps, (max_x - min_x + 1), (max_y - min_y + 1) *)
  start, steps, (w, h), (min_x, min_y), (max_x, max_y)
;;


let estimate_size steps =
  let (w, h) = steps |> List.fold_left (fun ((x, y), (max_x, max_y)) (dir, len) ->
    Printf.printf "estimate_size: pos: (%d, %d);\n" x y;
    assert (x >= 0 && y >= 0 && max_x >= 0 && max_y >= 0);
    let (x, y) = advance_pos len (x,y) dir in
    (* Printf.printf "estimate_size.step: %d %d\n" x y ; *)
    ((x, y), (max max_x x, max max_y y))
  ) ((0,0), (0,0)) |> snd in
  (w+1, h+1)
;;



type tile_reachability = Inside | Outside | Path ;;

let draw_map map =
  map |> Array.iter (fun l ->
    l |> Array.iter (fun t ->
      match t with
        | Inside -> print_string "#"
        | Outside -> print_string "."
        | Path -> print_string "#"
    ) ;
    print_newline ()
  )
;;


let reachability (path: (int*int) list) (map: bool array array) =
  (* let path, _ = find_loop map in *)
  (* Printf.printf "reachability\n%!" ; *)
  let h, w = Array.length map, Array.length map.(0) in
  let valid_pos x y = x >= 0 && y >= 0 && x < w && y < h in
  let is_border x y = x = 0 || y = 0 || x = w - 1 || y = h - 1 in
  (* Printf.printf "reachability %d %d\n" w h ; *)
  let map' = map |> Array.mapi (fun y -> Array.mapi (fun x t ->
    (* if not (List.mem (x,y) path) then Inside else *)
    (* Printf.printf "%d, %d\n%!" x y ; *)
    assert ((List.mem (x,y) path) = (map.(y).(x) = true)) ;
    match t with
      | false -> if is_border x y then Outside else Inside
      | true -> Path
  )) in
  (* Printf.printf "map before reachability:\n%!" ; *)
  (* draw_map map' ; *)
  let rec imp x y path =
    if (not (valid_pos x y)) || (path |> List.tl |> List.mem (x,y)) then ()
    else begin
      [(1,0); (0,1); (-1,0); (0,-1)]
        |> List.map (fun (x', y') -> (x+x', y+y'))
        |> List.filter (fun (x, y) -> valid_pos x y)
        |> List.iter (fun (x, y) ->
          match map'.(y).(x) with
            | Path -> ()
            | Outside -> ()
            | Inside ->
                map'.(y).(x) <- Outside ;
                imp x y ((x,y)::path) ;
        )
    end
  in
  (* let w, h = dims map in *)
  for y = 0 to h-1 do
    for x = 0 to w-1 do
      if is_border x y && not (List.mem (x,y) path) then imp x y [(x,y)]
    done
  done;
  (* Printf.printf "map after reachability:\n%!" ; *)
  (* draw_map map' ; *)
  map'
    |> Array.mapi (fun y l -> (y, l |> Array.mapi (fun x t -> (x, t))))
    |> Array.fold_left (fun acc (y, l) ->
      acc + (l |> Array.fold_left (fun acc (x, t) ->
        (* acc + if is_real_coord x y && t = Inside then 1 else 0 *)
        (* acc + if t = Inside then 1 else 0 *)
        acc + match t with Inside | Path -> 1 | Outside -> 0
      ) 0)
    ) 0
;;


(* let pt01 steps =
  let w, h = estimate_size steps in
  (* Printf.printf "estimated size %d %d\n%!" h w ; *)
  let valid_pos (x,y) = x >= 0 && x < w && y >= 0 && y < h in
  let visited = Array.make_matrix h w false in
  let rec imp (x,y) steps =
    Printf.printf "imp %d %d\n%!" x y ;
    visited.(y).(x) <- true;
    match steps with
    | [] -> ()
    | (dir, len)::xs ->
      begin
        let next_pos = advance_pos1 (x,y) dir in
        assert(valid_pos next_pos);
        imp next_pos (if len = 1 then xs else (dir, len-1)::xs)
      end
  in imp (0,0) steps ;
  (* Printf.printf "map after visiting:\n%!" ; *)
  (* draw_map (visited |> Array.map (Array.map (function true -> Path | false -> Outside))) ; *)
  let path = get_path (0,0) steps in
  (* Printf.printf "path:\n%!" ; *)
  (* path |> List.iter (fun (x,y) -> Printf.printf "%d %d\n%!" x y) ; *)
  reachability (get_path (0,0) steps) visited
;; *)


let run steps =
  let start, steps, (w, h), (min_x, min_y), (max_x, max_y) = normalise_steps steps in
  let valid_pos (x,y) =
    Printf.printf "valid_pos %d %d\n%!" x y ;
    Printf.printf "x >= min_x: %b ; x <= max_x: %b ; y >= min_y: %b ; y <= max_y: %b\n%!" (x >= min_x) (x <= max_x) (y >= min_y) (y <= max_y) ;
    x >= min_x && x <= max_x && y >= min_y && y <= max_y
  in
  let visited = Array.make_matrix h w false in
  let rec imp (x,y) steps =
    Printf.printf "imp %d %d\n%!" x y ;
    visited.(y).(x) <- true;
    match steps with
    | [] -> ()
    | (dir, len)::xs ->
      begin
        let next_pos = advance_pos1 (x,y) dir in
        (* Printf.printf "next_pos: %d %d\n%!" (fst next_pos) (snd next_pos) ; *)
        (* assert(valid_pos next_pos); *)
        imp next_pos (if len = 1 then xs else (dir, len-1)::xs)
      end
  in imp start steps ;
  (* Printf.printf "map after visiting:\n%!" ; *)
  (* draw_map (visited |> Array.map (Array.map (function true -> Path | false -> Outside))) ; *)
  Printf.printf "get_path\n%!" ;
  let path = get_path start steps in
  (* Printf.printf "path:\n%!" ; *)
  (* path |> List.iter (fun (x,y) -> Printf.printf "%d %d\n%!" x y) ; *)
  reachability path visited
;;


(* Implementation strongly inspired by https://github.com/BenjaminDecker/advent-of-code/blob/main/src/18/solution.jl *)

let run' steps =
  let path, path_length = (
    let rec imp (path, len) = function
      | [] -> (path, len)
      | (dir, len')::xs ->
        let cur = List.hd path in
        imp ((advance_pos len' cur dir)::path, len+len') xs
      in
      imp ([0,0], 0) steps
  ) in
  let shoelace' (x,y) (x',y') = x*y' - x'*y in
  let shoelace points =
    let rec imp acc = function
      | [] -> acc
      | [x] -> acc + shoelace' x (List.hd points)
      | x::y::xs -> imp (acc + shoelace' x y) (y::xs)
    in
    imp 0 (points |> drop_end 1)
  in
  (abs (shoelace path / 2)) - path_length / 2 + 1 + path_length
;;


let pt01 steps =
  steps
    |> List.map (fun (dir, len, _, _) -> (dir, len))
    |> run'
;;


let pt02 steps =
  steps
    |> List.map (fun (_, _, dir, len) -> (dir, len))
    |> run'
;;

let () =
  let input = read_lines "input.txt" in
  let steps = input |> List.map parse_step in
  steps |> pt01 |> Printf.printf "Pt01: %d\n%!" ;
  steps |> pt02 |> Printf.printf "Pt02: %d\n%!" ;
