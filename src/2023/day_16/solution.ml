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



type tile = Empty | Mirror_FwdSlash | Mirror_BackSlash | SplitterH | SplitterV


let parse_input (input: string list) =
  let w, h = List.length input, String.length (List.hd input) in
  input
    |> List.map (fun row ->
      row
        |> get_chars
        |> List.map (function '.' -> Empty | '/' -> Mirror_FwdSlash | '\\' -> Mirror_BackSlash | '-' -> SplitterH | '|' -> SplitterV | _ -> failwith "invalid char")
        |> Array.of_list
    )
    |> Array.of_list
;;



let dims g = Array.length g, Array.length g.(0) ;;


let check_pos grid x y =
  let h, w = dims grid in
  x >= 0 && x < w && y >= 0 && y < h
;;


type dir = Up | Down | Left | Right ;;

type beam = dir * int * int ;;


let advance_pos x y = function
  | Up    -> x, y-1
  | Down  -> x, y+1
  | Left  -> x-1, y
  | Right -> x+1, y
;;


module BeamSet = Set.Make(struct
  type t = beam
  let compare = compare
end)


let calc_energised grid start_beam =
  let h, w = dims grid in
  let energised = Array.make_matrix h w false in
  let max_unchanged_steps = max h w in
  let imp: beam list -> (int * beam list) = function
    | [] -> 0, []
    | beams -> begin
      beams |> List.fold_left (fun (num_changed, next_beams) (dir, x, y) ->
        let num_changed = num_changed + if energised.(y).(x) then 0 else (energised.(y).(x) <- true ; 1) in
        let aux dir =
          let x', y' = advance_pos x y dir in
          (num_changed, (dir, x', y') :: next_beams)
        in
        let aux' dirs =
          let beams = List.map (fun dir -> let x', y' = advance_pos x y dir in dir, x', y') dirs in
          (num_changed, beams @ next_beams)
        in
        match dir, grid.(y).(x) with
          | _, Empty -> aux dir
          | Right, Mirror_FwdSlash -> aux Up
          | Left, Mirror_FwdSlash -> aux Down
          | Up, Mirror_FwdSlash -> aux Right
          | Down, Mirror_FwdSlash -> aux Left
          | Right, Mirror_BackSlash -> aux Down
          | Left, Mirror_BackSlash -> aux Up
          | Up, Mirror_BackSlash -> aux Left
          | Down, Mirror_BackSlash -> aux Right
          | Right, SplitterH | Left, SplitterH -> aux dir
          | Up, SplitterV | Down, SplitterV -> aux dir
          | Right, SplitterV | Left, SplitterV ->
            aux' [Up; Down]
          | Up, SplitterH | Down, SplitterH ->
            aux' [Left; Right]
      ) (0, [])
      end
  in
  let rec _imp seen_beams = function
    | (0, _) -> ()
    | (remaining_steps, bs) -> begin
      let new_seen = BeamSet.union seen_beams (BeamSet.of_list bs) in
      match imp (bs |> List.filter (fun ((dir, x, y) as beam) -> check_pos grid x y && not (BeamSet.mem beam seen_beams))) with
        | _, [] -> ()
        | 0, bs -> _imp new_seen (remaining_steps - 1, bs)
        | _, bs -> _imp new_seen (max_unchanged_steps, bs)
      end
  in
  _imp BeamSet.empty (max_unchanged_steps, [start_beam]) ;
  energised
;;



let calc_energised_sum grid start_beam =
  let energised = calc_energised grid start_beam in
  energised |> Array.fold_left (fun acc row -> acc + Array.fold_left (fun acc b -> if b then acc + 1 else acc) 0 row) 0
;;


let pt01 grid =
  (* grid |> calc_energised |> Array.fold_left (fun acc row -> acc + Array.fold_left (fun acc b -> if b then acc + 1 else acc) 0 row) 0 *)
  (* let energised = calc_energised grid (Right, 0, 0) in
  Array.iter (fun row -> Array.iter (fun b -> Printf.printf "%s" (if b then "#" else ".")) row ; Printf.printf "\n") energised ;
  energised |> Array.fold_left (fun acc row -> acc + Array.fold_left (fun acc b -> if b then acc + 1 else acc) 0 row) 0 *)
  calc_energised_sum grid (Right, 0, 0)
;;


let mk_range_excl a b =
  if a > 0 then [] else
  let rec imp acc = function
    | x when x = b -> acc
    | x -> imp (x :: acc) (x+1)
  in imp [] a
;;


let flat_map f l =
  let rec imp acc = function
    | [] -> acc
    | x :: xs -> imp (f x @ acc) xs
  in imp [] l
;;


let pt02 grid =
  let h, w = dims grid in
  let start_beams = (
      List.init w (fun x -> (Down, x, 0))
    @ List.init w (fun x -> (Up, x, h-1))
    @ List.init h (fun y -> (Right, 0, y))
    @ List.init h (fun y -> (Left, w-1, y))
  ) in
  start_beams |> List.map (calc_energised_sum grid) |> List.fold_left max 0
;;


let () =
  let input = read_lines "input.txt" in
  let grid = parse_input input in
  grid |> pt01 |> Printf.printf "pt01: %d\n%!" ;
  grid |> pt02 |> Printf.printf "pt02: %d\n%!" ;
