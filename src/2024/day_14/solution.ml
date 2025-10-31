module PosSet = Set.Make(struct
  type t = int*int
  let compare = compare
end) ;;


let read_lines filename =
  let file = open_in filename in
  let rec imp acc = 
    try imp (input_line file :: acc)
    with End_of_file -> close_in file; List.rev acc
  in
  imp []
;;



type bot = (int * int) * (int * int) ;;


let parse lines =
  let parse_coord s =
    let s = String.sub s 2 (String.length s - 2) in
    match String.split_on_char ',' s with
      | [x;y] -> int_of_string x, int_of_string y
      | _ -> failwith "invalid input"
  in
  lines |> List.map (fun s ->
    match String.split_on_char ' ' s with
      | [p;v] -> (
        parse_coord p, parse_coord v
      )
      | _ -> failwith "invalid input!"
  )
  |> Array.of_list
;;



let rec wrap n dim =
  if n < 0 then wrap (dim + n) dim else
  if n >= dim then wrap (n - dim) dim
  else n
;;


let simulate1 (width, height) ((x,y), (x',y')) =
  ((wrap (x-x') width, wrap (y-y') height), (x',y'))
;;



let rec simulate size bots = function
  | 0 -> ()
  | n -> (
    bots |> Array.map_inplace (fun bot -> simulate1 size bot) ;
    simulate size bots (n-1)
  )
;;




type quadrant = TopLeft | TopRight | BottomLeft | BottomRight ;;

let string_of_quadrant = function
  | TopLeft -> "TopLeft"
  | TopRight -> "TopRight"
  | BottomLeft -> "BottomLeft"
  | BottomRight -> "BottomRight"
;;


let quadrant (width, height) x y =
  let mid_x = width / 2 in
  let mid_y = height / 2 in
  if x = mid_x || y = mid_y then None
  else Some (
    match x < mid_x, y < mid_y with
      | true, true -> TopLeft
      | true, false -> BottomLeft
      | false, true -> TopRight
      | false, false -> BottomRight
  )
;;

let pt01 size bots =
  simulate size bots 100 ;
  let tl,tr,bl,br = bots |> Array.fold_left (fun (tl,tr,bl,br) ((x,y),_) ->
    match quadrant size x y with
      | None             -> (tl,tr,bl,br)
      | Some TopLeft     -> (tl+1,tr,bl,br)
      | Some TopRight    -> (tl,tr+1,bl,br)
      | Some BottomLeft  -> (tl,tr,bl+1,br)
      | Some BottomRight -> (tl,tr,bl,br+1)
  ) (0,0,0,0)
  in
  tl * tr * bl * br
;;




(* PART 2 *)


let clear_lines num_lines =
  for _ = 1 to num_lines do
    Printf.printf "\027[A\027[K"
  done;
  flush stdout
;;



let range a b =
  Seq.unfold (fun a -> if a >= b then None else Some (a,a+1)) a
;;


let dump_bots_per_tile (width, height) bots =
  range 0 height |> Seq.iter (fun y ->
    range 0 width |> Seq.iter (fun x ->
      let num_bots = bots |> Array.fold_left (fun acc (pos, _) -> acc + if pos = (x,y) then 1 else 0) 0 in
      if num_bots > 0 then
        print_int num_bots
      else
        print_char '.'
    ) ;
    Printf.printf "\n" ;
  )
;;


let simulate' size bots f =
  let rec imp n =
    bots |> Array.map_inplace (fun bot -> simulate1 size bot) ;
    if f n bots then
      (* (Unix.sleep 1 ; *)
      imp (n+1)
    else
      n
  in
  imp 1 ;
;;


let has_line (width, height) bots =
  let bots = bots |> Array.fold_left (fun acc (pos,_) -> PosSet.add pos acc) PosSet.empty in
  range 0 height |> Seq.exists (fun y ->
    let span = range 0 width |> Seq.fold_left (fun (max, cur_len) x ->
      let cur_has_bot = PosSet.mem (x,y) bots in
      match cur_len, cur_has_bot with
        | None, false -> max, None
        | Some cur_len, true -> max, Some (cur_len+1)
        | None, true -> max, Some 1
        | Some cur_len, false -> Stdlib.max max cur_len, None
    ) (0, None) |> fst
    in
    span > 30
  )
;;


let pt02 size bots =
  let print_bots n bots =
    Printf.printf "After %i sec:\n" n ;
    dump_bots_per_tile size bots ;
    flush stdout ;
  in
  print_bots 0 bots ;
  simulate' size bots (fun n bots ->
    (* clear_lines (Array.length bots + 1) ;
    print_bots n bots ; *)
    if has_line size bots then (
      Printf.printf "%i\n%!" n ;
      (* print_bots n bots ; *)
      true)
    else
      true
  )
;;


(*
6416
16819
27222
37625
48028
*)




let () =
  let size, bots = (101, 103), "input.txt" |> read_lines |> parse in
  pt01 size bots |> Printf.printf "pt01: %i\n%!" ;
  (* Printf.printf "1\n2\n3\n4\n5\n%!" ;
  clear_lines 1 ;
  Printf.printf "A\n%!" ; *)
  pt02 size bots |> Printf.printf "pt02: %i\n%!" ;
  ()