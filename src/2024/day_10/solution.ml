module PosSet = Set.Make(struct
  type t = int*int
  let compare = compare
end)

module PathSet = Set.Make(struct
  type t = (int*int) list
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



let range a b =
  Seq.unfold (fun a -> if a >= b then None else Some (a,a+1)) a
;;



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




let find_trails size map =
  let get x y =
    let offset = y * size + x in
    map.(offset)
  in
  let valid_pos x y =
    x >= 0 && x < size && y >= 0 && y < size
  in
  let starting_points = Seq.product (range 0 size) (range 0 size)
    |> Seq.filter (fun (x,y) -> get x y = 0)
  in
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
  starting_points
    |> Seq.map (fun (x,y) -> find_trails [] [] x y)
    |> List.of_seq
    |> List.filter ((<>) [])
;;



let score trails =
  trails |> List.map (fun trail -> trail |> List.rev |> List.hd) |> PosSet.of_list |> PosSet.cardinal
;;


let rating trails =
  trails |> PathSet.of_list |> PathSet.cardinal
;;



let () =
  Printexc.record_backtrace true ;
  let input = read_lines "input.txt" in
  let size, map = parse input in
  let run f =
    find_trails size map |> List.fold_left (fun acc trails -> acc + f trails) 0
  in
  run score |> Printf.printf "pt01: %i\n%!" ;
  run rating |> Printf.printf "pt02: %i\n%!" ;
