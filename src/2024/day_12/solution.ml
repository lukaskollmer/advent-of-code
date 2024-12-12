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
  regions
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
  region |> count_where (fun (x',y',_) -> is_neighbour (x,y) (x',y'))
;;

let perimeter region =
  region |> List.fold_left (fun acc (x,y,_) -> acc + (4 - count_neighbours (x,y) region)) 0
;;

let price region =
  (area region) * (perimeter region)
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


let dump_regions (regions: (int*int*char) list list): unit =
  regions |> List.iteri (fun idx region ->
    let _, _, c = List.hd region in
    Printf.printf "- [%c]: " c ;
    region |> List.iter (fun (x,y,_) -> Printf.printf "(%i, %i); " x y) ;
    let area = area region in
    let perimeter = perimeter region in
    Printf.printf "--> (area: %i, perimeter: %i) -> price: %i\n%!" area perimeter (area * perimeter) ;
  )
;;



let () =
  let size, map = "input.txt" |> read_lines |> parse in
  let regions = find_regions1 size map in
  Printf.printf "#regions: %i\n%!" (List.length regions) ;
  dump_regions regions ;
  regions |> List.fold_left (fun acc r -> acc + price r) 0 |> Printf.printf "pt01: %i\n%!" ;
  ()