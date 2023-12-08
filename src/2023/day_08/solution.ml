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




type direction = L | R

let parse_direction = function 'L' -> L | 'R' -> R ;;


type node = String

module NodeMap = Map.Make(String)

type node_map = (node * node) NodeMap.t


let parse_input input =
  let directions = input |> List.hd |> get_chars |> List.map parse_direction in
  let rest = input |> List.tl |> List.tl in
  let map = rest |> List.fold_left (fun acc line ->
    let name = String.sub line 0 3 in
    let l = String.sub line 7 3 in
    let r = String.sub line 12 3 in
    NodeMap.add name (l, r) acc
  ) NodeMap.empty in
  (directions, map)
;;




let opt_get = function Some x -> x | None -> failwith "None" ;;


let walk map directions =
  let rec imp dst cur directions =
    if cur = dst then []
    else
      let (d, directions) = directions |> Seq.uncons |> opt_get in
      let (l, r) = NodeMap.find cur map in
      match d with
        | L -> l::(imp dst l directions)
        | R -> r::(imp dst r directions)
  in
  imp "ZZZ" "AAA" (directions |> List.to_seq |> Seq.cycle)
;;


let pt01 input =
  let (directions, map) = parse_input input in
  let steps = walk map directions in
  List.length steps
;;



let pt02 input = 12 ;;


let () =
  let input = read_lines "input.txt" in
  input |> pt01 |> Printf.printf "Pt01: %i\n" ;
  input |> pt02 |> Printf.printf "Pt01: %i\n" ;
