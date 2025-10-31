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


let parse input =
  let size = List.length input in
  let map = input |> List.map (fun row ->
    Array.init size (fun i -> row.[i])
  ) |> Array.of_list
  in
  size, map
;;



let range a b =
  Seq.unfold (fun a -> if a >= b then None else Some (a,a+1)) a
;;


let range_from a =
  Seq.unfold (fun a -> Some (a,a+1)) a
;;


let get map x y =
  map.(y).(x)
;;



let list_product l1 l2 =
  Seq.product (List.to_seq l1) (List.to_seq l2) |> List.of_seq
;;

let list_map_product f l1 l2 =
  Seq.map_product f (List.to_seq l1) (List.to_seq l2) |> List.of_seq
;;


type dir = Up | Down | Left | Right ;;

let next_in_dir (x,y) = function
  | Up -> (x,y-1)
  | Down -> (x,y+1)
  | Left -> (x-1,y)
  | Right -> (x+1,y)
;;



let third (_,_,c) = c ;;


let find_regions1 size map =
  let indices = range 0 size in
  let valid (x,y) =
    x >= 0 && y >= 0 && x < size && y < size
  in
  let regions = Seq.map_product (fun x y ->
    [(x, y, get map x y)]
  ) indices indices |> List.of_seq in
  let rec imp seen acc c_exp (x,y) =
    if PosSet.mem (x,y) seen then seen, acc else
    match get map x y with
      | c when c <> c_exp -> seen, acc
      | c -> (
        [Up; Down; Left; Right]
        |> List.map (next_in_dir (x,y))
        |> List.filter valid
        |> List.fold_left (fun (seen,acc) pos ->
          imp seen acc c_exp pos
        ) (PosSet.add (x,y) seen, (x,y,c)::acc)
      )
  in
  let regions = Seq.product indices indices
    |> Seq.fold_left (fun (seen,acc) (x,y) ->
        let seen, region = imp seen [] (get map x y) (x,y) in
        seen, region::acc
      ) (PosSet.empty, [])
    |> snd |> List.filter ((<>) [])
  in
  (* Printf.printf "#regions: %i\n%!" (List.length regions) ;
  dump_regions regions ; *)
  regions |> List.map (fun region -> region |> List.hd |> third, region |> List.map (fun (x,y,_) -> (x,y)))
;;



let area region = List.length region ;;


let count_where f l =
  let rec imp acc = function
    | [] -> acc
    | x::xs -> imp (acc + if f x then 1 else 0) xs
  in
  imp 0 l
;;

let is_neighbour p1 p2 =
  let next_in_dir = next_in_dir p1 in
  p2 = next_in_dir Up || p2 = next_in_dir Down || p2 = next_in_dir Left || p2 = next_in_dir Right
;;

let count_neighbours (x,y) region =
  region |> count_where (fun (x',y') -> is_neighbour (x,y) (x',y'))
;;

let perimeter region =
  region |> List.fold_left (fun acc (x,y) -> acc + (4 - count_neighbours (x,y) region)) 0
;;

let price1 region =
  (area region) * (perimeter region)
;;

let all_pos (x,y,max_x,max_y) =
  Seq.product (range x (max_x+1)) (range y (max_y+1)) |> PosSet.of_seq
;;

let sides region =
  let outer_positions = region |> List.filter_map (fun (x,y) -> if count_neighbours (x,y) region < 4 then Some (x,y) else None) in
  let min_x, min_y, max_x, max_y = outer_positions |> List.fold_left (fun (min_x, min_y, max_x, max_y) (x,y) ->
    min x min_x, min y min_y, max x max_x, max y max_y
  ) (let (x,y) = List.hd outer_positions in (x,y,x,y)) in
  (* outer_positions |> List.fold_left (fun acc (x,y) -> acc + if (not (List.mem (next_in_dir (x,y) Up) outer_positions)) then 1 else 0) 0 *)
  let rec rects seen acc = function
    | [] -> acc
    | (x,y)::rest -> (
      if PosSet.mem (x,y) seen then rects seen acc rest else
        let max_x = range_from x |> Seq.find (fun x' -> not (List.mem (x',y) outer_positions)) |> Option.get |> (+) (-1) in
        (* let max_y = range_from y |> Seq.find_map (fun y' ->
          if List.mem (x,y') outer_positions then None else Some (
            (* we have found the first y that is "below" the outer poss reachable by going directly down from x *)
            let y' = y' - 1 in
            range_from y |> Seq.find (fun y' -> range )
          )) *)
        let max_y = range_from y |> Seq.find (fun y' ->
          range x (max_x + 1) |> Seq.for_all (fun x -> List.mem (x,y') outer_positions) |> not
        ) |> Option.get |> (+) (-1) in
        rects (PosSet.add_seq (Seq.product (range x (max_x+1)) (range y (max_y+1))) seen) ((x,y,max_x,max_y)::acc) rest
    )
  in
  let rects = rects PosSet.empty [] outer_positions in
  (* Printf.printf *)
  12
;;

(* X
  XXX
   X
*)




let sides region =
  let outer_positions = region |> List.filter_map (fun (x,y) -> if count_neighbours (x,y) region < 4 then Some (x,y) else None) |> PosSet.of_list in
  let is_corner pos = (* is outer corner? (ie, is a piece which is surrounded by empty tiles at 3 sides)*)
    let has_neighbour_at dir =
      List.mem (next_in_dir pos dir) region
    in
    (* count_neighbours pos region = 1 || count_neighbours pos region = 3 *)
    let is1 = has_neighbour_at in 12
  in
  (* PosSet.fold (fun pos acc -> acc + if is_corner pos then 1 else 0) outer_positions 4 *)
  12
;;





type axis = Horizontal | Vertical ;;

let sides size region =
  let edge_positions = region |> List.filter_map (fun (x,y) -> if count_neighbours (x,y) region < 4 then Some (x,y) else None) |> PosSet.of_list in
  let has_neighbour_at pos dir =
    List.mem (next_in_dir pos dir) region
  in
  let outer_edge_positions = edge_positions |> PosSet.filter (fun (x,y) ->
    x = 0 || y = 0 || x = size-1 || y = size-1 || (
      
    )
  ) in
  let min_x, min_y, max_x, max_y = edge_positions |> List.fold_left (fun (min_x, min_y, max_x, max_y) (x,y) ->
    min x min_x, min y min_y, max x max_x, max y max_y
  ) (let (x,y) = List.hd edge_positions in (x,y,x,y))
  in
  let has_h_neighbour (x,y) =
    has_neighbour_at Left || has_neighbour_at
  in
  let h_sides_by_y = (
    let h = Hashtbl.create 12 in
    edge_positions |> List.iter (fun (x,y) ->
      let positions = Hashtbl.find_opt h y |> Option.value ~default:[] in
      Hashtbl.add h y ((x,y)::positions) ;
    ) ;
    h
  ) in
  let rec rects seen acc = function
    | [] -> acc
    | (x,y)::rest -> (
      if PosSet.mem (x,y) seen then rects seen acc rest else
        let max_x = range_from x |> Seq.find (fun x' -> not (List.mem (x',y) edge_positions)) |> Option.get |> (+) (-1) in
        (* let max_y = range_from y |> Seq.find_map (fun y' ->
          if List.mem (x,y') outer_positions then None else Some (
            (* we have found the first y that is "below" the outer poss reachable by going directly down from x *)
            let y' = y' - 1 in
            range_from y |> Seq.find (fun y' -> range )
          )) *)
        let max_y = range_from y |> Seq.find (fun y' ->
          range x (max_x + 1) |> Seq.for_all (fun x -> List.mem (x,y') outer_positions) |> not
        ) |> Option.get |> (+) (-1) in
        rects (PosSet.add_seq (Seq.product (range x (max_x+1)) (range y (max_y+1))) seen) ((x,y,max_x,max_y)::acc) rest
    )
  in
  let rects = rects PosSet.empty [] outer_positions in
  (* Printf.printf *)
  12
;;





let price2 region =
  (area region) * (sides region)
;;

(* let find_regions2 map =
  let rec imp acc =
    match acc with
      | [] -> ( (* at the start *)
        failwith "TODO"
      )
      | (x,y)::acc -> ( (* already in area *)

      )
  in
;; *)


let dump_regions regions =
  regions |> List.iteri (fun idx (c,region) ->
    Printf.printf "- [%c]: " c ;
    region |> List.iter (fun (x,y) -> Printf.printf "(%i, %i); " x y) ;
    Printf.printf "\n  --> (area: %i, perimeter: %i, sides: %i) -> price1: %i; price2: %i\n%!" (area region) (perimeter region) (sides region) (price1 region) (price2 region) ;
  )
;;



let () =
  let size, map = "input.txt" |> read_lines |> parse in
  let regions = find_regions1 size map in
  Printf.printf "#regions: %i\n%!" (List.length regions) ;
  dump_regions regions ;
  regions |> List.fold_left (fun acc (_,r) -> acc + price1 r) 0 |> Printf.printf "pt01: %i\n%!" ;
  regions |> List.fold_left (fun acc (_,r) -> acc + price2 r) 0 |> Printf.printf "pt02: %i\n%!" ;
  ()