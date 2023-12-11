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

let get_opt = function Some x -> x | None -> failwith "None" ;;

let last_char s = s.[String.length s - 1] ;;


let rec gcd u v =
  if v <> 0 then gcd v (u mod v)
  else abs u
;;

let lcm m n =
  match m, n with
  | 0, _ | _, 0 -> 0
  | m, n -> abs (m * n) / (gcd m n)
;;

type dir = Left | Right ;;


module NodeMap = Map.Make(String) ;;


let parse_input input =
  let dirs = input |> List.hd |> get_chars |> List.map (function 'L' -> Left | 'R' -> Right) in
  let nodes = input |> List.tl |> List.tl |> List.map (fun l ->
    String.sub l 0 3, String.sub l 7 3, String.sub l 12 3
  ) in
  let nodes' = nodes |> List.fold_left (fun map (n, l, r) ->
    NodeMap.add n (l, r) map
  ) NodeMap.empty in
  (dirs, nodes')
;;


let run dirs nodes is_initial is_final =
  let dirs = dirs |> List.to_seq |> Seq.cycle in
  let rec imp dirs d node =
    if is_final node then d
    else
      let dir, dirs = dirs |> Seq.uncons |> get_opt in
      match dir with
        | Left -> imp dirs (d+1) (fst (NodeMap.find node nodes))
        | Right -> imp dirs (d+1) (snd (NodeMap.find node nodes))
  in
  (* imp initial dirs 0 *)
  let initial = nodes |> NodeMap.to_list |> List.filter_map (fun (k, _) -> if is_initial k then Some k else None) in
  initial |> List.map (imp dirs 0) |> List.fold_left lcm 1
;;

let pt01 input =
  let dirs, nodes = parse_input input in
  run dirs nodes ((=) "AAA") (fun s -> last_char s = 'Z')
;;

let pt02 input =
  let dirs, nodes = parse_input input in
  run dirs nodes (fun s -> last_char s = 'A') (fun s -> last_char s = 'Z')
;;

let () =
  let input = read_lines "input.txt" in
  input |> pt01 |> Printf.printf "Pt01: %d\n" ;
  input |> pt02 |> Printf.printf "Pt02: %d\n" ;
