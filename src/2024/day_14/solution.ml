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
;;



let range a b =
  Seq.unfold (fun a -> if a >= b then None else Some (a,a+1)) a
;;


let dump_bots_per_tile (width, height) bots =
  range 0 height |> Seq.iter (fun y ->
    range 0 width |> Seq.iter (fun x ->
      let num_bots = bots |> List.fold_left (fun acc (pos, _) -> acc + if pos = (x,y) then 1 else 0) 0 in
      if num_bots > 0 then
        print_int num_bots
      else
        print_char '.'
    ) ;
    Printf.printf "\n%!" ;
  )
;;



let rec wrap n dim =
  (* Printf.printf "wrap %i %i\n%!" n dim ; *)
  if n < 0 then wrap (dim + n) dim else
  if n >= dim then wrap (n - dim) dim
  else n
;;


let rec simulate (width, height) ((x,y), (x',y') as bot) = function
  | 0 -> bot
  | n -> (
    (* Printf.printf "\nAt n = %i\n%!" n ;
    dump_bots_per_tile (width, height) [bot] ; *)
    let bot = ((wrap (x+x') width, wrap (y+y') height), (x',y')) in
    simulate (width, height) bot (n-1)
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
    (* Printf.printf "SOME QUADRANT\n%!" ; *)
    let q = match x < mid_x, y < mid_y with
      | true, true -> TopLeft
      | true, false -> BottomLeft
      | false, true -> TopRight
      | false, false -> BottomRight
    in
    Printf.printf "%i, %i -> %s\n%!" x y (string_of_quadrant q) ;
    q
  )
;;

let pt01 size bots =
  Printf.printf "At start:\n%!" ;
  dump_bots_per_tile size bots ;
  let bots = bots |> List.map (fun bot -> simulate size bot 100) in
  Printf.printf "At n = 100:\n%!" ;
  dump_bots_per_tile size bots ;
  let tl,tr,bl,br = bots |> List.fold_left (fun (tl,tr,bl,br) ((x,y),_) ->
    match quadrant size x y with
      | None             -> (tl,tr,bl,br)
      | Some TopLeft     -> (tl+1,tr,bl,br)
      | Some TopRight    -> (tl,tr+1,bl,br)
      | Some BottomLeft  -> (tl,tr,bl+1,br)
      | Some BottomRight -> (tl,tr,bl,br+1)
  ) (0,0,0,0)
  in
  Printf.printf "(tl: %i, tr: %i, bl: %i, br: %i)\n%!" tl tr bl br ;
  tl * tr * bl * br
;;



let () =
  let size, bots = (101, 103), "input.txt" |> read_lines |> parse in
  (* let size, bots = (11, 7), "input1.txt" |> read_lines |> parse in *)
  (* let bots = [(2,4), (2,-3)] in *)
  pt01 size bots |> Printf.printf "pt01: %i\n%!" ;
  ()