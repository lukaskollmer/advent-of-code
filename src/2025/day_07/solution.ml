let read_lines filename =
  let file = open_in filename in
  let rec imp acc = 
    try imp (input_line file :: acc)
    with End_of_file -> close_in file; List.rev acc
  in
  imp []
;;


module IntPair = struct
  type t = int * int
  let compare (a1,a2) (b1,b2) =
    if Int.(equal a1 b1 && equal a2 b2) then 0 else if a1 < b1 then -1 else 1
  
  let equal (a1,a2) (b1,b2) = Int.(equal a1 b1 && equal a2 b2)

  let hash (x,y) = x lxor y
end

module PosSet = Set.Make(IntPair) ;;


type tile = Free | Beam | Splitter ;;

let tile_of_char = function
  | '.' -> Free
  | 'S' | '|' -> Beam
  | '^' -> Splitter
  | _ -> failwith ""
;;

let parse_input lines =
  lines |> List.to_seq |> Seq.map (fun l ->
    l |> String.to_seq |> Seq.map (tile_of_char) |> Array.of_seq
  ) |> Array.of_seq
;;

let range a b =
  Seq.unfold (fun a -> if a >= b then None else Some (a,a+1)) a
;;


let pt01 m =
  let (h, w) = (Array.length m, Array.length m.(0)) in
  let step y m =
    if y = h-1 then PosSet.empty else
    range 0 w |> Seq.fold_left (fun acc x ->
      match (m.(y).(x), m.(y+1).(x)) with
        | Free, _ | Splitter, Free -> acc
        | Beam, Free -> PosSet.add (x,y+1) acc
        | Beam, Splitter -> acc |> PosSet.add (x-1,y+1) |> PosSet.add (x+1,y+1)
        | Beam, Beam | Splitter, _ -> failwith ""
    ) PosSet.empty
  in
  range 0 h |> Seq.iter (fun y ->
    let pos = step y m in
    pos
      |> PosSet.filter (fun (x,y) -> x > 0 && y > 0 && x < w && y < h)
      |> PosSet.iter (fun (x,y) -> m.(y).(x) <- Beam)
  ) ;
  range 1 h |> Seq.fold_left (fun acc y ->
    range 0 w |> Seq.fold_left (fun acc x ->
      acc + if m.(y).(x) = Splitter && m.(y-1).(x) = Beam then 1 else 0
    ) acc
  ) 0
;;


module H = Hashtbl.Make(IntPair) ;;

let memo_rec (f: (int * int -> int) -> int * int -> int) =
  let h = H.create 16 in
  let rec g x =
    try H.find h x
    with Not_found ->
      let y = f g x in
      H.add h x y;
      y
  in
  g
;;


let pt02 m =
  let h = Array.length m in
  let rec imp self (x,y): int =
    if y = h-1 then 1 else (
    match m.(y+1).(x) with
      | Free -> self (x, (y+1))
      | Splitter -> self ((x-1), (y+1)) + self ((x+1), (y+1))
      | Beam -> failwith ""
    )
  in
  let imp = memo_rec imp in
  imp (m.(0) |> Array.find_index ((=) Beam) |> Option.get, 0)
;;


let () =
  let input = read_lines "input.txt" in
  input |> parse_input |> pt01 |> Printf.printf "Pt01: %i\n%!" ;
  input |> parse_input |> pt02 |> Printf.printf "Pt02: %i\n%!" ;
