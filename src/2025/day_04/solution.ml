let read_lines filename =
  let file = open_in filename in
  let rec imp acc = 
    try imp (input_line file :: acc)
    with End_of_file -> close_in file; List.rev acc
  in
  imp []
;;


type tile = Free | Roll ;;

let tile_of_char = function
  | '.' -> Free
  | '@' -> Roll
  | _ -> failwith ""
;;

let parse_input (lines: string list) =
  lines |> List.map (fun line ->
    line |> String.to_seq |> Seq.map tile_of_char |> Array.of_seq
  ) |> Array.of_list
;;


let range a b =
  Seq.unfold (fun a -> if a >= b then None else Some (a,a+1)) a
;;


let count_where f l =
  let rec imp acc = function
    | [] -> acc
    | x::xs -> imp (acc + if f x then 1 else 0) xs
  in
  imp 0 l
;;


let dims m =
  let h = Array.length m in
  let w = Array.length m.(0) in
  (w, h)
;;

let adjacent_pos m x y =
  let (w, h) = dims m in
  [
    (x-1, y-1);
    (x,   y-1);
    (x+1, y-1);
    (x+1, y  );
    (x+1, y+1);
    (x,   y+1);
    (x-1, y+1);
    (x-1, y  );
  ] |> List.filter (fun (x,y) -> x >= 0 && y >= 0 && x < w && y < h)
;;


let copy m =
  let (h, w) = dims m in
  Array.init_matrix h w (fun y x -> m.(y).(x))
;;


let run m limit =
  let (h, w) = dims m in
  let rec imp acc = function
    | 0 -> acc
    | limit ->
      let acc' = range 0 h |> Seq.fold_left (fun acc y ->
        range 0 w |> Seq.fold_left (fun acc x ->
          match m.(y).(x) with
            | Free -> acc
            | Roll ->
              let adj_pos = adjacent_pos m x y in
              let num_adj = adj_pos |> count_where (fun (x,y) -> m.(y).(x) = Roll) in
              if num_adj < 4 then (x,y)::acc else acc
        ) acc
      ) acc in
      acc' |> List.iter (fun (x,y) -> m.(y).(x) <- Free) ;
      imp acc' (if acc' = acc then 0 else (limit-1))
  in
  imp [] limit
;;


let pt01 m =
  run m 1 |> List.length
;;

let pt02 m =
  run m Int.max_int |> List.length
;;



let () = 
  let m = read_lines "input.txt" |> parse_input in
  m |> copy |> pt01 |> Printf.printf "Pt01: %i\n%!" ;
  m |> copy |> pt02 |> Printf.printf "Pt02: %i\n%!" ;
