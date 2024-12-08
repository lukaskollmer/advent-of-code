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



let parse lines =
  lines |> List.fold_left (fun (y,s) row ->
    let _, s = row |> String.fold_left (fun (x,s) -> function
      | '.' -> (x+1,s)
      | c -> (x+1, Seq.cons (x,y,c) s)
    ) (0,s) in
    (y+1,s)
  ) (0,Seq.empty)
;;



let dump_map size antennas antinodes =
  let map = Array.make_matrix size size '.' in
  antennas |> Seq.iter (fun (x,y,c) -> map.(y).(x) <- c) ;
  let antinodes = List.of_seq antinodes in
  map |> Array.iteri (fun y row ->
    row |> Array.iteri (fun x tile ->
      let c = if List.mem (x,y) antinodes then '#' else tile in
      print_char c ;
    ) ;
    print_newline () ;
  )
;;


let valid_pos size (x,y) =
  x >= 0 && y >= 0 && x < size && y < size
;;



let antinodes1 size p1 p2 =
  let imp (x,y) (x',y') =
    let (d,d') = (x-x', y-y') in
    (x+d, y+d')
  in
  Seq.(cons (imp p1 p2) (cons (imp p2 p1) empty))
;;


let antinodes2 size p1 p2 =
  let imp (x,y) (x',y') =
    let (d,d') = (x-x', y-y') in
    Seq.unfold (fun (x,y) ->
      let next = (x+d,y+d') in
      if valid_pos size next then (Some (next,next)) else None
    ) (x,y) |> Seq.cons (x,y)
  in
  Seq.append (imp p1 p2) (imp p2 p1)
;;




let run size antennas antinodes  =
  (* dump_map map Seq.empty ; *)
  let antinodes = Seq.map_product (fun (x,y,c) (x',y',c') ->
      if c <> c' || (x,y) = (x',y') then None else
        Some (antinodes size (x,y) (x',y'))
    ) antennas antennas
    |> Seq.flat_map (function
      | None -> Seq.empty
      | Some seq -> seq |> Seq.filter (valid_pos size)
    )
  in
  (* dump_map size antennas antinodes ; *)
  antinodes |> PosSet.of_seq |> PosSet.cardinal
;;


let () =
  let size, antennas = "input.txt" |> read_lines |> parse in
  let run = run size antennas in
  run antinodes1 |> Printf.printf "pt01: %i\n%!" ;
  run antinodes2 |> Printf.printf "pt02: %i\n%!" ;
