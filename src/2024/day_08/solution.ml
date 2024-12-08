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



module PosSet = Set.Make(struct 
  type t = int*int
  let compare = compare
end)

let id x = x ;;

type tile = Empty | Antenna of char ;;

let tile_of_char = function
  | '.' -> Empty
  | c -> Antenna c
;;


let parse lines =
  lines
  |> List.map (fun line -> line |> List.map tile_of_char |> Array.of_list)
  |> Array.of_list
;;



let get_antennas map =
  let size = Array.length map in
  Seq.unfold (fun (x,y) ->
    if y >= size then None else
    Some ((x, y, map.(y).(x)), (if x = size-1 then (0,y+1) else (x+1,y)))
  ) (0,0) |> Seq.filter (function (_, _, Empty) -> false | (_, _, Antenna _) -> true)
;;


let comp_antinode_pos (x,y) (x',y') =
  let diff = (x-x', y-y') in
  (x+fst diff, y+snd diff)
;;


let dump_map map antinodes =
  let antinodes = List.of_seq antinodes in
  map |> Array.iteri (fun y row ->
    row |> Array.iteri (fun x tile ->
      let c = if List.mem (x,y) antinodes then '#' else (match tile with Empty -> '.' | Antenna c -> c) in
      print_char c ;
    ) ;
    print_newline () ;
  )
;;


let pt01 map =
  let size = Array.length map in
  (* dump_map map Seq.empty ; *)
  let antennas = get_antennas map in
  let antinodes = Seq.map_product (fun (x,y,c) (x',y',c') ->
      if c <> c' || (x,y) = (x',y') then None else
        Some [comp_antinode_pos (x,y) (x',y'); comp_antinode_pos (x',y') (x,y)]
    ) antennas antennas
    |> Seq.filter_map (id)
    |> Seq.flat_map (List.to_seq)
    |> Seq.filter (fun (x,y) -> x >= 0 && y >= 0 && x < size && y < size)
  in
  (* dump_map map antinodes ; *)
  antinodes |> PosSet.of_seq |> PosSet.cardinal


let () =
  Printexc.record_backtrace true ;
  let input = read_lines "input.txt" in
  let map = input |> List.map get_chars |> parse in
  map |> pt01 |> Printf.printf "pt01: %i\n%!" ;