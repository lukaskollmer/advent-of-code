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


let rec transpose = function
  | [] -> []
  | []::xss -> transpose xss
  | (x::xs)::xss -> List.((x :: map hd xss) :: transpose (xs :: map tl xss))
;;


let rotate l = l |> transpose |> List.map List.rev ;;


let count a = List.fold_left (fun acc x -> if x = a then acc + 1 else acc) 0 ;;


let take n l =
  let rec imp acc = function
    | (0, _) -> List.rev acc
    | (_, []) -> List.rev acc
    | (n, x::xs) -> imp (x::acc) (n-1, xs)
  in imp [] (n, l)
;;

let drop n l =
  let rec imp = function
    | (0, xs) -> xs
    | (_, []) -> []
    | (n, x::xs) -> imp (n-1, xs)
  in imp (n, l)
;;

let drop_end n l =
  l |> List.rev |> drop n |> List.rev
;;


let split_while f l =
  let rec imp acc = function
    | [] -> (List.rev acc, [])
    | x::xs when f x -> imp (x::acc) xs
    | xs -> (List.rev acc, xs)
  in imp [] l
;;



type tile = MovableRock | FixedRock | Empty ;;
let parse_tile = function 'O' -> MovableRock | '#' -> FixedRock | '.' -> Empty ;;


let rec tilt_rocks = function
  | [] -> []
  | (MovableRock::xs) -> begin
      let (pre, post) = split_while ((=) Empty) (tilt_rocks xs) in
      match pre with
        | [] -> MovableRock::post
        | xs -> xs @ [MovableRock] @ post
    end
  | x::xs -> x :: (tilt_rocks xs)
;;

let calc_weight l =
  l |> List.mapi (fun i r -> count MovableRock r * (i+1)) |> List.fold_left (+) 0
;;

let pt01 input = 
  input
    |> rotate
    |> List.map tilt_rocks
    |> rotate
    |> calc_weight
;;


let cycle input =
  input
    |> List.map tilt_rocks (* tilt towards north *)
    |> rotate
    |> List.map tilt_rocks (* tilt towards west *)
    |> rotate
    |> List.map tilt_rocks (* tilt towards south *)
    |> rotate
    |> List.map tilt_rocks (* tilt towards east *)
    |> rotate
;;




module CycleHashTbl = Hashtbl.Make(struct
  type t = tile list list
  let equal (a: t) (b: t) = a = b
  let hash l = l |> List.mapi (fun i r -> (i, r))
    |> List.fold_left (fun acc (i, x) ->
      let h = x
        |> List.mapi (fun i t -> (i, t))
        |> List.fold_left (fun acc (i, t) -> acc lxor ((match t with Empty -> 1 | MovableRock -> 2 | FixedRock -> 3) lsl i)) 0
      in
      acc lxor (h lsl i)
  ) 0
end) ;;


let find_cycle input =
  let h = CycleHashTbl.create 1000 in
  let rec imp acc input =
    match CycleHashTbl.find_opt h input with
      | Some _ -> List.rev (input::acc)
      | None -> begin
          let r = cycle input in
          CycleHashTbl.add h input r ;
          imp (r::acc) r
        end
  in
  imp [] input
;;




let pt02 n input =
  let path = input |> rotate |> find_cycle in
  let fst_idx = path
    |> List.find_index (fun x -> x = List.nth path (List.length path - 1))
    |> get_opt
  in
  Printf.printf "Cycle length: %d\n%!" (List.length path - fst_idx) ;
  Printf.printf "MOD: %d\n%!" (List.length path - 1 - fst_idx -1) ;
  (* let idx = fst_idx + ((n - fst_idx) mod (List.length path - 1 - fst_idx)) in *)
  let idx = ((n - fst_idx) mod ((List.length path - fst_idx) - 1)) -1 in
  List.nth path idx |> rotate |> calc_weight
;;


let () =
  let input = read_lines "input.txt"
    |> List.map get_chars
    |> List.map (List.map parse_tile)
  in
input |> pt01 |> Printf.printf "Pt01: %d\n%!" ;
input |> pt02 1000000000 |> Printf.printf "Pt02: %d\n%!" ;
