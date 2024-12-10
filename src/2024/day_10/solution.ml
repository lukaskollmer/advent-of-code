module PosSet = Set.Make(struct
  type t = int*int
  let compare = compare
end)


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


let id x = x ;;

let get size map x y =
  let offset = y * size + x in
  map.(offset)
;;

let set size map x y v =
  let offset = y * size + x in
  map.(offset) <- v
;;


let range a b =
  Seq.unfold (fun a -> if a >= b then None else Some (a,a+1)) a



let parse input =
  let size = List.length input in
  let map = input
    |> List.to_seq
    |> Seq.map String.to_seq
    |> Seq.flat_map (Seq.map (function '.' -> 100 | c -> int_of_char c - int_of_char '0'))
    |> Array.of_seq
  in
  size, map
;;


type direction = Up | Down | Left | Right ;;

let next_in_dir x y = function
  | Up -> (x,y-1)
  | Down -> (x,y+1)
  | Left -> (x-1,y)
  | Right -> (x+1,y)
;;

let valid_pos size x y =
  x >= 0 && x < size && y >= 0 && y < size
;;

let pt01 size map =
  let get = get size map in
  let valid_pos = valid_pos size in
  let starting_points = Seq.product (range 0 size) (range 0 size)
    |> Seq.filter (fun (x,y) -> get x y = 0)
  in
  (* Printf.printf "starting_points:\n%!" ; *)
  (* starting_points |> Seq.iter (fun (x,y) -> Printf.printf "- [%02i, %02i]\n%!" x y) ; *)
  let rec find_trails cur acc x y =
    let next_steps () =
      [Up; Down; Left; Right]
        |> List.fold_left (fun acc dir ->
          let x', y' = next_in_dir x y dir in
          find_trails ((x,y)::cur) acc x' y'
        ) acc
    in
    if not (valid_pos x y) then acc else
    match cur with [] -> next_steps () | (x',y')::_ -> (
      if get x y <> get x' y' + 1 then acc else
      if get x y = 9 then (List.rev ((x,y)::cur))::acc else
      next_steps ()
    )
  in
  let trails_by_head = starting_points
    |> Seq.map (fun (x,y) -> find_trails [] [] x y)
    |> List.of_seq
    |> List.filter ((<>) [])
  in
  (* Printf.printf "#trails: %i\n%!" (List.length trails_by_head) ;
  trails_by_head |> List.iteri (fun i l ->
    Printf.printf "- [%i]:\n%!" i ;
    l |> List.iteri (fun i l ->
      Printf.printf "  - [%i]:" i ;
      l |> List.iter (fun (x,y) -> Printf.printf "(%i,%i)=%i; " x y (get x y)) ;
      print_newline () ;
    ) ;
  ) ;
  Printf.printf "#trailheads: %i\n%!" (List.length trails_by_head) ; *)
  (* trails |> List.fold_left (fun acc trails -> acc + List.length trails) 0 *)
  trails_by_head |> List.fold_left (fun acc trails ->
    let x = trails |> List.map (fun trail -> trail |> List.rev |> List.hd) |> PosSet.of_list |> PosSet.cardinal in
    acc + x
  ) 0
;;



let () =
  Printexc.record_backtrace true ;
  let input = read_lines "input.txt" in
  let size, map = parse input in
  pt01 size map |> Printf.printf "pt01: %i\n%!" ;
