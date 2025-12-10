(* ocamlopt -g -O3 -I +unix unix.cmxa solution.ml *)

let read_lines filename =
  let file = open_in filename in
  let rec imp acc = 
    try imp (input_line file :: acc)
    with End_of_file -> close_in file; List.rev acc
  in
  imp []
;;

(* let measure l f =
  let start = Unix.gettimeofday () in
  let result = f () in
  let stop = Unix.gettimeofday () in
  Printf.printf "[MEASURE] %s took %f sec\n%!" l (stop -. start) ;
  result
;; *)


let parse_input lines =
  lines |> List.map (fun s ->
    let nth = List.nth (String.split_on_char ',' s) in
    (int_of_string (nth 0), int_of_string (nth 1))
  )
;;


let size p1 p2 =
  let (x1,y1) = p1 in
  let (x2,y2) = p2 in
  let size = (x2+1-x1) * (y2+1-y1) in
  Int.abs size
;;

let product_fold f acc a b =
  List.fold_left (fun acc a ->
    List.fold_left (fun acc b -> f acc a b) acc b
  ) acc a
;;

let pt01 ps =
  product_fold (fun acc p1 p2 -> Int.max acc (size p1 p2)) 0 ps ps
;;


type tile = Free | Red | Green ;;

let range a b =
  Seq.unfold (fun a -> if a >= b then None else Some (a,a+1)) a
;;


module CoordSet = Set.Make(struct
  type t = int * int
  let compare p1 p2 =
    Pair.compare Int.compare Int.compare p1 p2
end) ;;


let next_towards p1 p2 =
  if p1 = p2 then p1 else
  let (x1,y1) = p1 in
  let (x2,y2) = p2 in
  assert (not (p1=p2)) ;
  assert (x1=x2 || y1=y2) ;
  if x1=x2 then
    (* same row *)
    x1, (if y2 > y1 then y1+1 else y1-1)
  else
    (* same col *)
    (if x2 > x1 then x1+1 else x1-1), y1
;;


let chunked n l =
  if n = 0 then [l] else
  let rec imp acc = function
    | [] -> List.rev acc
    | l -> imp ((List.take n l)::acc) (List.drop n l)
  in imp [] l
;;

let chunked' n l =
  let size = List.length l / n in
  Printf.printf "chunk_size: %i\n%!" size ;
  chunked size l
;;

let chunked'' l = chunked' (Domain.recommended_domain_count ()) l;;

let fixpoint f x =
  let rec imp x p =
    if x = p then x else imp (f x) x
  in
  imp (f x) x
;;

let min2 a b =
  if Int.compare a b > 0 then (b,a) else (a,b)
;;

let fold_left_adj f acc l =
  let rec imp acc = function
    | [] | _::[] -> acc
    | x1::x2::xs -> imp (f acc x1 x2) (x2::xs)
  in
  imp acc l
;;



type 'a fold_left_adj_state = Continue of 'a | Done of 'a ;;


let fold_left_adj' f acc l =
  let rec imp acc = function
    | [] | _::[] -> acc
    | x1::x2::xs ->
      (* imp (f acc x1 x2) (x2::xs) *)
      match f acc x1 x2 with
        | Continue s -> imp s (x2::xs)
        | Done s -> s
  in
  imp acc l
;;

(* let adj s =
  Seq.unc
  Seq.unfold (fun a ) *)

let pt02 red =
  let (w,h) = red |> List.fold_left (fun (w,h) (x,y) -> (Int.max w x), (Int.max h y)) (0,0) in
  (* assert (w=h) ; *)
  Printf.printf "w = %i; h = %i\n%!" w h ;
  let all = (
    let rec imp acc = function
      | [] -> acc
      | p::[] -> imp (CoordSet.add p acc) []
      | p1::p2::ps ->
        let next = next_towards p1 p2 in
        (* Printf.printf "next from (%i,%i) towards (%i,%i) = (%i,%i)\n%!" (fst p1) (snd p1) (fst p2) (snd p2) (fst next) (snd next) ; *)
        if next = p1 then
          imp (CoordSet.add p1 acc) (p2::ps)
        else
          imp (CoordSet.add p1 acc) (next::p2::ps)
    in
    imp CoordSet.empty (red @ [List.hd red])
  ) in
  (* Printf.printf "#all = %i\n%!" (CoordSet.cardinal all) ;
  range 0 (h+2) |> Seq.iter (fun y ->
    range 0 (w+2) |> Seq.iter (fun x ->
      Printf.printf "%c" (if CoordSet.mem (x,y) all then 'X' else '.') ;
      ) ;
      Printf.printf "\n" ;
  ) ; *)
  let all' = all in
  (* Printf.printf "will compute all\n%!" ; *)
  let w2 = w / 2 in
  let h2 = h / 2 in
  let process p1 p2 all =
    let (x1,x2) = p1 in
    let (y1,y2) = p2 in
    Seq.product (range x1 (Int.min x2 (w+1))) (range y1 (Int.min y2 (h+1))) |> Seq.fold_left (fun all (x,y) ->
    let enclosed_in_row () =
      let range = if x < w2 then range 0 x else range (x+1) (w+1) in 
      ((range |> Seq.fold_left (fun (prev,acc) x ->
        (* let prev' = CoordSet.mem(prev,y) all' in *)
        let x' = CoordSet.mem(x,y) all' in
        let acc = match prev,x' with
          | false,true -> acc + 1
          | _ -> acc
        in
        (x',acc)
      ) (false,0)) |> snd) mod 2 = 1
    in
    let enclosed_in_col () =
      let range = if y < h2 then range 0 y else range (y+1) (h+1) in
      ((range |> Seq.fold_left (fun (prev,acc) y ->
      (* ((range 0 (y+0) |> Seq.fold_left (fun (prev,acc) y -> *)
        (* let prev' = CoordSet.mem(x,prev) all' in *)
        let y' = CoordSet.mem(x,y) all' in
        let acc = match prev,y' with
          | false,true -> acc + 1
          | _ -> acc
        in
        (y',acc)
      ) (false,0)) |> snd) mod 2 = 1
    in
    if enclosed_in_row () && enclosed_in_col () then
      CoordSet.add (x,y) all
    else
      all
  ) all
  in
  let in_path x y =
    let module L = struct
      type t = Done of bool | InProgress
    end in
    let r = fold_left_adj' (fun c (bx,by) (ax,ay) -> (
        if x = ax && y = ay then
          Done true
        else if (ay > y) <> (by > y) then (
          let slope = (x - ax) * (by - ay) - (bx - ax) * (y - ay) in
          if slope = 0 then
            Done true
          else if (slope < 0) <> (by < ay) then
            Continue (not c)
          else Continue c
        ) else
          Continue c
    )) false (all' |> CoordSet.to_list) in
    r
  in
  let memo f =
    (* let module H = Hashtbl.Make(CoordPair) in *)
    let h = Hashtbl.create 11 in
    fun x ->
      try Hashtbl.find h x
      with Not_found ->
        let y = f x in
        Hashtbl.add h x y;
        y
  in
  (* let in_path (a,b) = in_path a b in
  let in_path = memo in_path in
  let in_path a b = in_path (a,b) in *)
  (* let process' () =
    range 0 (h+1) |> Seq.fold_left (fun all y ->
      let _,_,all = range 0 (w+1) |> Seq.fold_left (fun (i2,i1, all) x -> 
        let i0 = CoordSet.mem (x,y) all' in
        let iN = CoordSet.mem ((x+1),y) all' in
        match i2, i1, i0 with
          | false, false, false -> (i1, false, all)
          | false, false, true -> (i1, true, all)
          | false, true, false -> (i1, true, CoordSet.add (x,y) all)
          | false, true, true -> (i1, true, all)
          | true, false, false -> (i1, false, all)
          | true, false, true -> (i1, true, all)
          | true, true, false -> (i1, true, CoordSet.add (x,y) all)
          | true, true, true -> (i1, true, all)
          (* | false, false, true | true, false, true -> (i1, i0, CoordSet.add (x,y) all)
          | true, true, false | false, true, false -> (i1, i0, all) *)
          (* | true -> (i1,true, if is_in then all else CoordSet.add (x,y) all) *)
          (* | false -> () *)
      ) (false,false, all) in
      all
    ) all
  in *)
  let process' () =
    range 0 (h+1) |> Seq.fold_left (fun all y ->
      let all = range 0 (w+1) |> Seq.fold_left (fun all x ->
        Printf.printf "%i, %i\n%!" x y ;
        if in_path x y then CoordSet.add (x,y) all else all
      ) all in
      Printf.printf "%i of %i\n%!" y (h) ;
      all
    ) all
  in
  (* let all = process (0,w) (0,h) all in *)
  Printf.printf "will compute all\n%!" ;
  let all = process' () in
  (* Printf.printf "#domains: %i\n" (Domain.recommended_domain_count ()) ; *)
  (* let num_chunks = Domain.recommended_domain_count () in
  let chunk_size = h / num_chunks in
  let domains = range 0 num_chunks |> List.of_seq |> List.map (fun y ->
    Domain.spawn (fun () ->
      let (y1,y2) = (y * chunk_size, ((y+1) * chunk_size)) in
      Printf.printf "will run for chunk %i (%i - %i)\n%!" y y1 y2;
      let r = process (0,w+1) (y1,y2) all in
      Printf.printf " did run for chunk %i\n%!" y ;
      r
    )
  ) in
  let all = domains |> List.fold_left (fun acc d ->
    CoordSet.union acc (Domain.join d)
  ) all in *)
  Printf.printf " did compute all\n%!" ;
  (* let all = fixpoint step all in *)
  (* Printf.printf "#all = %i\n%!" (CoordSet.cardinal all) ;
  range 0 (h+2) |> Seq.iter (fun y ->
    range 0 (w+2) |> Seq.iter (fun x ->
      Printf.printf "%c" (if CoordSet.mem (x,y) all then 'X' else '.') ;
      ) ;
      Printf.printf "\n" ;
  ) ; *)
  let points p1 p2 =
    let (x1,y1) = p1 in
    let (x2,y2) = p2 in
    if x1 = x2 then
      let (y1,y2) = min2 y1 y2 in
      range y1 (y2+1) |> Seq.map (fun y -> (x1,y))
    else if y1 = y2 then
      let (x1,x2) = min2 x1 x2 in
      range x1 (x2+1) |> Seq.map (fun x -> (x,y1))
    else
      let (min_x, max_x) = min2 x1 x2 in
      let (min_y, max_y) = min2 y1 y2 in
      range min_y (max_y+1) |> Seq.concat_map (fun y ->
        range min_x (max_x+1) |> Seq.map (fun x -> (x,y))
      )
  in
  let valid p1 p2 =
    points p1 p2 |> Seq.for_all (fun (x,y) -> CoordSet.mem (x,y) all)
  in
  Printf.printf "will run the main part\n%!" ;
  product_fold (fun acc p1 p2 ->
    if valid p1 p2 then 
      Int.max acc (size p1 p2)
    else
      acc
  ) 0 red red
;;

let () =
  let input = read_lines "input.txt" |> parse_input in
  input |> pt01 |> Printf.printf "Pt01: %i\n%!" ;
  input |> pt02 |> Printf.printf "Pt02: %i\n%!" ;
