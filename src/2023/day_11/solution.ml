module IntSet = Set.Make(Int) ;;

module PosPairsSet = Set.Make(struct type t = ((int*int)*(int*int)) let compare = compare end) ;;


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

let opt_get = function Some x -> x | None -> failwith "None." ;;
let opt_get' msg = function Some x -> x | None -> failwith msg ;;

let rec all f = function
  | [] -> true
  | x::xs -> f x && all f xs
;;

let rec flat_map f = function
  | [] -> []
  | x::xs -> f x @ flat_map f xs
;;

let rec insert_at n x = function
  | [] -> []
  | y::ys -> if n = 0 then x::y::ys else y::insert_at (n-1) x ys
;;

let mk_range_excl a b =
  let rec imp acc = function
    | x when x = b -> List.rev acc
    | x -> imp (x::acc) (x+1)
  in
  imp [] a
;;

let find_shortest xs =
  xs |> List.fold_left (fun acc x -> match acc with None -> Some x | Some x' -> Some (if List.length x' < List.length x then x' else x)) None
;;


type element = Empty | Galaxy ;;


let parse_input input =
  input |> List.map (fun l -> l |> get_chars |> List.map (function '.' -> Empty | '#' -> Galaxy))
;;


let dump_universe univ =
  (* univ |> List.map (fun l -> l |> List.map (function Empty -> '.' | Galaxy -> '#') |> String.concat "") |> String.concat "\n" *)
  (* let h = List.length univ in
  let w = List.length (List.hd univ) in
  let row_indices = mk_range_excl 0 h |> List.map string_of_int in
  let col_indices = mk_range_excl 0 w |> List.map string_of_int in
  let row_idx_max_len = row_indices |> List.map String.length |> List.fold_left max 0 in
  let col_idx_max_len = col_indices |> List.map String.length |> List.fold_left max 0 in *)
  (* Printf.printf "%*s %s\n" col_idx_max_len "" (col_indices |> String.concat " ") ; *)
  (* Printf.printf "%*s " row_idx_max_len "" ;
  col_indices |> List.iter x ->
  ) ; *)
  univ |> List.iteri (fun y l ->
    Printf.printf "%3d " y ;
    l |> List.iter (fun e -> Printf.printf "%s" (match e with Empty -> "." | Galaxy -> "#")) ;
    Printf.printf "\n" ;
  ) ;
;;


let enlargen univ =
  let rec imp = function
    | [] -> []
    | x::xs -> x::(if all ((=) Empty) x then x::imp xs else imp xs)
  in
  let univ = imp univ in
  let rec imp univ = function
    | -1 -> univ
    | n ->
      if all (fun l -> List.nth l n = Empty) univ
      (* then imp (map |> flat_map (insert_at n Empty)) (n-1) *)
      then imp (univ |> List.map (insert_at n Empty)) (n-1)
      else imp univ (n-1)
  in
  let univ = imp univ (List.length (List.hd univ) - 1) in
  assert (univ |> List.map List.length |> IntSet.of_list |> IntSet.cardinal = 1) ;
  univ
;;


let find_galaxies univ =
  univ
    |> List.mapi (fun y l -> l |> List.mapi (fun x e -> (x,y),e))
    |> List.flatten
    |> List.filter (fun (_,e) -> e = Galaxy)
    |> List.map fst
;;


let univ_to_array univ =
  Printf.printf "univ_to_array\n%!";
  Printf.printf "univ_to_array (#input = %d; %d)\n%!" (List.length univ) (0);
  Printf.printf "univ_to_array (#input = %d; %d)\n%!" (List.length univ) (univ |> List.map List.length |> List.fold_left (+) 0);
  Array.of_list (univ |> List.map Array.of_list)
;;


let univ_dims univ = (Array.length univ, Array.length univ.(0)) ;;


let rec inspect f = function
  | [] -> []
  | x::xs -> f x ; x::inspect f xs
;;


(* let str_repeat n str = String.concat "" (Seq.) *)


let shortest_path univ (x_src, y_src) (x_dst, y_dst) =
  abs (x_src - x_dst) + abs (y_src - y_dst)
;;


let pt01 input =
  (* Printf.printf "UNIV PRE-ENLARGEN:\n" ; *)
  (* dump_universe input ; *)
  let univ = input |> enlargen in
  (* Printf.printf "UNIV POST-ENLARGEN:\n" ; *)
  (* dump_universe univ ; *)
  Printf.printf "will find galaxies\n%!" ;
  let galaxies = find_galaxies univ in
  Printf.printf "did find %d galaxies\n%!" (List.length galaxies) ;
  (* galaxies |> List.iter (fun (x,y) -> Printf.printf "(%i, %i)\n" x y) ; *)
  Printf.printf "will calc galaxies\n%!" ;
  let pairs = galaxies |> List.fold_left (fun pairs (g1: (int*int)) ->
    galaxies |> List.fold_left (fun pairs g2 ->
      if g1 = g2 || PosPairsSet.mem (g1,g2) pairs || PosPairsSet.mem (g2,g1) pairs then pairs else PosPairsSet.add (g1,g2) pairs
    ) pairs
  ) PosPairsSet.empty in
  (* let pairs = galaxies |> List.fold_left (fun pairs g1 ->
    galaxies |> List.fold_left (fun pairs g2 ->
      if g1 = g2 || List.mem (g1,g2) pairs || List.mem (g2,g1) pairs then pairs else (g1,g2)::pairs
    ) pairs
  ) [] in *)
  Printf.printf "will run univ_to_arrayyyy\n%!" ;
  let univ = univ_to_array univ in
  Printf.printf "will run the shortest_path stuff\n%!" ;
  (* pairs |> List.iter (fun (g1,g2) -> Printf.printf "PAIR (%i, %i) -> (%i, %i)\n" (fst g1) (snd g1) (fst g2) (snd g2)) ; *)
  pairs |> PosPairsSet.to_list |> List.map (fun (src,dst) -> shortest_path univ src dst) |> List.fold_left (+) 0
;;


let pt02 input = 12 ;;


let () =
  let input = read_lines "input.txt" |> parse_input in
  input |> pt01 |> Printf.printf "Pt01: %d\n" ;
  input |> pt02 |> Printf.printf "Pt02: %d\n" ;
