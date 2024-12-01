let read_lines filename =
  let file = open_in filename in
  let rec imp acc = 
    try imp (input_line file :: acc)
    with End_of_file -> close_in file; List.rev acc
  in
  imp []
;;



let (>>) f g = fun x -> g (f x) ;;

let opt_get = function Some x -> x | None -> failwith "None" ;;



let drop n l =
  let rec imp n = function
    | [] -> []
    | _ :: xs when n > 0 -> imp (n-1) xs
    | xs -> xs
  in
  imp n l
;;

let take n l =
  let rec imp n acc = function
    | [] -> List.rev acc
    | x :: xs when n > 0 -> imp (n-1) (x :: acc) xs
    | _ -> List.rev acc
  in
  imp n [] l
;;


let rec transpose = function
  | [] -> []
  | []::xss -> transpose xss
  | (x::xs)::xss -> List.((x :: map hd xss) :: transpose (xs :: map tl xss))
;;


let rotate l = l |> transpose |> List.map List.rev ;;



type dir = Up | Down | Left | Right ;;

let all_dirs = [Up; Down; Left; Right] ;;
let opposite_dir = function Up -> Down | Down -> Up | Left -> Right | Right -> Left ;;

type tile = Path | Forest | Slope of dir ;;


let parse_map input =
  let parse_tile = function
    | '.' -> Path
    | '#' -> Forest
    | '^' -> Slope Up
    | 'v' -> Slope Down
    | '<' -> Slope Left
    | '>' -> Slope Right
    | _ -> failwith "invalid tile"
  in
  Array.init (List.length input) (fun y ->
    Array.init (String.length (List.hd input)) (fun x ->
      parse_tile (List.nth input y).[x]
    )
  )
;;


let char_of_tile = function 
  | Path -> '.'
  | Forest -> '#'
  | Slope Up -> '^'
  | Slope Down -> 'v'
  | Slope Left -> '<'
  | Slope Right -> '>'
;;

let dump_map map draw_slopes =
  Array.iter (fun row ->
    row |> Array.iter ((if draw_slopes then char_of_tile else function Slope _ -> '.' | Forest -> '#' | Path -> '.') >> print_char) ;
    print_newline ()
  ) map
;;


let dump_path map path draw_slopes =
  Array.iteri (fun y row ->
    row |> Array.iteri (fun x tile ->
      if List.mem (x,y) path then print_char 'O'
      else print_char (if draw_slopes then char_of_tile tile else (match tile with Slope _ -> '.' | _ -> char_of_tile tile))
    ) ;
    print_newline ()
  ) map
;;


let dims map = Array.length map, Array.length map.(0) ;;

let advance_pos x y dir =
  match dir with
  | Up -> x, y - 1
  | Down -> x, y + 1
  | Left -> x - 1, y
  | Right -> x + 1, y
;;


module PosSet = Set.Make(struct
  type t = int * int
  let compare = compare
end) ;;

module PosHashtbl = Hashtbl.Make(struct
  type t = int * int
  let equal = (=)
  let hash = Hashtbl.hash
end) ;;



(* module T = Domainslib.Task *)




let find_paths ignore_slopes map =
  let h, w = dims map in
  Printf.printf "dims: w=%d h=%d\n%!" w h ;
  let pos_valid x y =
    x >= 0 && x < w && y >= 0 && y < h
  in
  let rec imp x y path next_dir =
    if not (pos_valid x y) || (PosSet.mem (x,y) path) then
      [path]
    else begin
      let all_dirs = match next_dir with None -> all_dirs | Some dir -> [dir] in
      all_dirs |> List.map (fun dir ->
          let x', y' = advance_pos x y dir in
          if not (pos_valid x' y') then []
          else match map.(y').(x') with
            | Slope dir' when (not ignore_slopes) && dir' = opposite_dir dir -> []
            | Forest -> []
            | _ ->
              let next_dir = if ignore_slopes then None else match map.(y').(x') with Slope dir -> Some dir | _ -> None in
              imp x' y' (PosSet.add (x,y) path) next_dir
        )
        (* |> List.fold_left (@) [] *)
        |> List.fold_left (fun acc paths ->
          let max_path_len = if acc = [] then 0 else (List.fold_left (fun acc path -> max acc (PosSet.cardinal path)) 0 acc) in
          paths |> List.fold_left (fun acc path ->
            if PosSet.cardinal path > max_path_len then [path]
            else acc
          ) acc
        ) []
    end
  in
  let start_x = Array.find_index ((=) Path) map.(0) |> opt_get in
  imp start_x 0 PosSet.empty None
;;


let pt01 map =
  let paths = map |> find_paths false |> List.sort (fun p1 p2 -> PosSet.cardinal p2 - PosSet.cardinal p1) in
  (List.hd paths |> PosSet.cardinal) - 1
;;




type graph = ((int * int) * int) list PosHashtbl.t ;;


let make_graph map =
  let h, w = dims map in
  let g = PosHashtbl.create ((h*w)/2) in
  let update_graph x y f =
    match PosHashtbl.find_opt g (x,y) with
      | None -> PosHashtbl.add g (x,y) (f None)
      | Some nodes -> PosHashtbl.replace g (x,y) (f (Some nodes))
  in
  let visited = PosHashtbl.create (h*w) in
  let pos_valid x y =
    x >= 0 && x < w && y >= 0 && y < h
  in
  let rec imp x y =
    if PosHashtbl.mem visited (x,y) then ()
    else begin
      PosHashtbl.add visited (x,y) () ;
      all_dirs
        |> List.map (fun dir -> let x', y' = advance_pos x y dir in (dir, x', y'))
        |> List.filter (fun (_, x', y') -> pos_valid x' y')
        |> List.iter (fun (dir, x', y') ->
            match map.(y').(x') with
            | Forest -> ()
            | _ -> update_graph x y (function
              | None -> [(x',y'), 1]
              | Some nodes -> ((x',y'), 1) :: nodes
            ) ; imp x' y'
          ) ;
          ()
    end
  in
  imp 0 0 ;
  g
;;




let all_nodes g =
  let visited = PosHashtbl.create (PosHashtbl.length g) in
  let rec imp acc s = match Seq.uncons s with
    | None -> acc
    | Some (pos, xs) when PosHashtbl.mem visited pos -> imp acc xs
    | Some (pos, xs) ->
      PosHashtbl.add visited pos () ;
      imp (PosSet.add pos acc) xs
  in
  imp PosSet.empty (PosHashtbl.to_seq_keys g)
;;


let flat_mapi f =
  let rec imp i acc = function
    | [] -> List.rev acc
    | x :: xs -> imp (i+1) (f i x :: acc) xs
in imp 0 []
;;



let _aux_extract_ranges l =
  let rec imp start idx = function
    | [] -> (match start with None -> [] | Some start -> [start, idx])
    | Forest::xs -> (match start with None -> imp None (idx+1) xs | Some start -> (start, idx) :: imp None (idx+1) xs)
    | _::xs -> imp (match start with None -> Some idx | Some start -> Some start) (idx+1) xs
  in
  imp None 0 l
;;



let process_graph' g map =
  let all_nodes = all_nodes g in
  let candidates = map |> Array.to_list |> flat_mapi (fun y row ->
    let ranges = _aux_extract_ranges (Array.to_list row) in
    Printf.printf "row: %s\n%!" (ranges |> List.map (fun (start, stop) -> Printf.sprintf "(%d, %d)" start stop) |> String.concat ", ") ;
    Some ranges
  ) in
  failwith "TODO" ;
;;

let process_graph g map =
  process_graph' g map ;
  failwith "TODO rotate and process again" ;
;;




module PriorityQueue2 = struct
  type priority = int

  type ('a) t = {
    mutable content: 'a queue
  }
  and 'a queue = Empty | Node of 'a t * (priority * 'a) * 'a t

  let empty: unit -> 'a t = fun _ -> {content = Empty}

  let fold (f: 'b -> (priority * 'a) -> 'b) (acc: 'b) (queue: 'a t): 'b =
    let rec imp acc = function
      | {content = Empty} -> acc
      | {content = Node(left, prio_elt, right)} ->
        let acc = imp acc left in
        let acc = f acc prio_elt in
        imp acc right
    in imp acc queue
  
  let size (queue: 'a t): int =
    fold (fun acc _ -> acc + 1) 0 queue

  let is_empty (queue: 'a t): bool = match queue with
    | {content = Empty} -> true
    | {content = Node(_, _, _)} -> false

  let max_depth queue =
    let rec imp = function
      | {content = Empty} -> 0
      | {content = Node(left, _, right)} -> 1 + max (imp left) (imp right)
    in imp queue

  let rec dump fmt_elem = function
    | {content = Empty} -> Printf.printf "{content=Empty}\n%!" ;
    | {content = Node(left, (prio, elt), right)} ->
      Printf.printf "{content=Node(" ;
      dump fmt_elem left ;
      Printf.printf ", (%d, %s), " prio (fmt_elem elt) ;
      Printf.printf "), " ;
      dump fmt_elem right ;
      Printf.printf ")}\n%!"

  let rec insert (_f: 'a -> string) (queue: 'a t) (prio: priority) (elt: 'a): unit =
    (* Printf.printf "-insert\n%!" ;
    Printf.printf "max depth: %d\n%!" (max_depth queue) ;
    dump _f queue ; *)
    match queue with
      | {content = Empty} ->
        (* Printf.printf "1st branch\n%!" ; *)
       queue.content <- Node(empty(), (prio, elt), empty()) ;
       (* Printf.printf "new val:\n%!" ; *)
       (* dump _f queue ; *)
      | {content = Node(left, (p, e), right)} ->
        (* Printf.printf "2nd branch (prio <= p: %b)\n%!" (prio <= p); *)
        if prio <= p
        then begin queue.content <- Node(right, (prio, elt), left) ; insert  _f right p e end
        else begin queue.content <- Node(right, (p, e), left) ; insert _f right prio elt end
        (* Printf.printf "new val:\n%!" ; *)
        (* dump _f queue *)

  let to_list_bad (queue: 'a t): 'a list =
    fold (fun acc (_, elt) -> elt :: acc) [] queue |> List.rev

  exception Queue_is_empty

  let rec remove_top (queue: 'a t): unit = match queue with
    | {content = Empty} -> raise Queue_is_empty
    | {content = Node(left, _, {content=Empty}) } -> queue.content <- left.content
    | {content = Node({content=Empty}, _, right) } -> queue.content <- right.content
    | {content = Node(({content = Node(_, (lprio, lelt), _)} as left), _, ({content = Node(_, (rprio, relt), _)} as right))} ->
      if lprio <= rprio
        then begin queue.content <- Node(left, (lprio, lelt), right) ; remove_top left
        end
        else begin
          queue.content <- Node(left, (rprio, relt), right) ; remove_top right
        end

  let extract queue: (priority * 'a) = match queue with
    | {content = Empty} -> raise Queue_is_empty
    | {content = Node(_, (prio, elt), _)} -> remove_top queue; (prio, elt)

  let rec mem (queue: 'a t) (elt: 'a) = match queue with
    | {content = Empty} -> false
    | {content = Node(left, (_, e), right)} ->
      if e = elt then true
      else mem left elt || mem right elt
  
  let rec find (queue: 'a t) (elt: 'a) = match queue with
    | {content = Empty} -> None
    | {content = Node(left, (prio, e), right)} ->
      if e = elt then Some (prio, e)
      else begin
        match find left elt with
        | Some x -> Some x
        | None -> find right elt
      end
    
    let rec find_where (queue: 'a t) (f: 'a -> bool) = match queue with
      | {content = Empty} -> None
      | {content = Node(left, (prio, e), right)} ->
        if f e then Some (prio, e)
        else begin
          match find_where left f with
          | Some x -> Some x
          | None -> find_where right f
        end

  let rec remove (queue: 'a t) (elt: 'a): unit =
    match queue with
      | {content = Empty} -> ()
      | {content = Node({content = Empty}, (_, e), {content = Empty})} when e = elt -> queue.content <- Empty
      | {content = Node(left, (_, e), {content = Empty})} when e = elt -> queue.content <- left.content
      | {content = Node({content = Empty}, (_, e), right)} when e = elt -> queue.content <- right.content
      | {content = Node(left, (prio, e), right)} ->
        if e = elt then remove_top queue
        else begin remove left elt ; remove right elt end
    (* let rec imp (queue: 'a t): unit = function
      | Empty -> Empty
      | Node(prio, e, left, right) ->
        if e = elt then remove_top queue
        else Node(prio, e, imp left, imp right)
    in imp queue *)

  let update_priority _f queue prio elt =
    Printf.printf "update_priority (%d, %s) (isin: %b)\n%!" prio (_f elt) (mem queue elt) ;
    if not (mem queue elt) then ()
    else begin
      Printf.printf "- size[initial]: %d\n%!" (size queue) ;
      let old_size = size queue in
      let was_in_queue = mem queue elt in
      remove queue elt ;
      Printf.printf "- size[after remove]: %d\n%!" (size queue) ;
      if was_in_queue then assert (not (mem queue elt)) ;
      if was_in_queue then assert (size queue = old_size - 1) ;
      insert _f queue prio elt ;
      Printf.printf "- size[after insert]: %d\n%!" (size queue) ;
      let new_size = size queue in
      assert (mem queue elt) ;
      Printf.printf "old_size: %d, new_size: %d\n%!" old_size new_size ;
      assert (new_size = old_size)
    end

  (* let update_priority queue prio elt =
    let rec imp parent queue = match queue with
      | {content = Empty} -> ()
      | {content = Node(left, (p, e), right)} ->
        if e <> elt then begin imp queue left; imp queue right end
        else begin
          (* e = elt *)

        end *)
end;;


module AStar = struct
  type 'a node = {
    data : 'a;
    mutable gscore : int;
    mutable fscore : int;
    mutable parent : 'a node option;
  }

  let _fmt_elem _ = "TODO" ;;

  let a_star_search start goal successors cost heuristic =
    let open_set = PriorityQueue2.empty () in
    let closed_set = Hashtbl.create 16 in

    let open_add node = PriorityQueue2.insert _fmt_elem open_set (node.fscore) node in
    let open_remove () = PriorityQueue2.extract open_set |> snd in
    let open_empty () = PriorityQueue2.is_empty open_set in

    let rec reconstruct_path node acc =
      match node.parent with
      | Some parent -> reconstruct_path parent (node.data :: acc)
      | None -> node.data :: acc
    in

    start.gscore <- 0;
    start.fscore <- heuristic start.data goal [];

    open_add start;

    let rec loop () =
      if open_empty () then None
      else
        let current = open_remove () in
        if current.data = goal then Some (reconstruct_path current [])
        else begin
          Hashtbl.add closed_set current.data ();
          let neighbors = successors current.data in
          neighbors |> List.iter (fun neighbor ->
            if not (Hashtbl.mem closed_set neighbor) then (
              let tentative_gscore = current.gscore + cost current.data neighbor in
              let neighbor_node =
                match PriorityQueue2.find_where open_set (fun node -> node.data = neighbor) with
                | Some (_, node) -> node
                | None -> { data = neighbor; gscore = max_int; fscore = max_int; parent = None }
              in
              if tentative_gscore < neighbor_node.gscore then (
                neighbor_node.gscore <- tentative_gscore;
                neighbor_node.fscore <- tentative_gscore + heuristic neighbor goal (reconstruct_path current []);
                neighbor_node.parent <- Some current;
                open_add neighbor_node
              )
          )) ;
          loop ()
        end
    in

    loop ()
end ;;



  



let pt02 map =
  (* let paths = map |> find_paths true |> List.sort (fun p1 p2 -> PosSet.cardinal p2 - PosSet.cardinal p1) in
  (List.hd paths |> PosSet.cardinal) - 1 *)
  let g = make_graph map in
  g |> PosHashtbl.iter (fun (x,y) l ->
    Printf.printf "(%d, %d) -> %s\n%!" x y (l |> List.map (fun ((x',y'), cost) -> Printf.sprintf "(%d, %d; %d)" x' y' cost) |> String.concat ", ")
  ) ;
  let heuristic_cost_estimate (x,y) (x',y') path =
    (* Printf.printf "\n\nHEURISTIC CALLED (%d, %d) --> (%d, %d)\n%!" x y x' y'; *)
    0
  in
  let cost (x,y) (x',y') =
    Printf.printf "COST CALLED (%d, %d) --> (%d, %d)\n%!" x y x' y';
    (* Printf.printf "path so far:\n%!" ;
    dump_path_in_grid grid path ; *)
    (* if not (check_no_four_conseq_blocks path) then max_int
    else grid.(y).(x) *)
    match PosHashtbl.find_opt g (x,y) with
      | None -> failwith "impossible" ; max_int
      | Some l -> List.assoc (x',y') l
  in
  let h, w = dims map in
  let neighbors_from (x,y) =
    let r = match PosHashtbl.find_opt g (x,y) with
      | None -> []
      | Some l -> l |> List.map fst
    in
    Printf.printf "Neighbours from (%d, %d): %s\n%!" x y (r |> List.map (fun (x,y) -> Printf.sprintf "(%d, %d)" x y) |> String.concat ", ") ;
    r
  in
  let start_x = Array.find_index ((=) Path) map.(0) |> opt_get in
  let start: (int*int) AStar.node = { data = (start_x,0); gscore = min_int; fscore = min_int; parent = None } in
  let goal = (w-2, h-1) in
  let path = AStar.a_star_search start goal neighbors_from cost heuristic_cost_estimate in
  (match path with
    | None -> failwith "Goal not reachable"
    | Some path -> begin
      List.iter (fun (x,y) -> Printf.printf "(%d, %d) -> " x y) path ;
      print_newline () ;
      dump_path map path false ;
      path |> List.length
    end
  )
;;



let other_dirs = function
  | Up -> [Left; Right; Down]
  | Down -> [Left; Right; Up]
  | Left -> [Up; Down; Right]
  | Right -> [Up; Down; Left]
;;
let ugh = function
  | Up -> [Up; Left; Right]
  | Down -> [Down; Left; Right]
  | Left -> [Left; Up; Down]
  | Right -> [Right; Up; Down]
;;


let max_by_len l1 l2 = if List.length l1 > List.length l2 then l1 else l2 ;;

let pt02 map =
  let h, w = dims map in
  let src = (1, 0) in
  let dst = (w-2, h-1) in
  let valid_pos x y =
    x >= 0 && x < w && y >= 0 && y < h
  in
  let visited = PosHashtbl.create (h*w) in
  let queue = Queue.create () in
  Queue.add (Down, [src], PosSet.singleton src) queue ;
  let cur_max_path = ref [] in
  while not (Queue.is_empty queue) do
    let prev_dir, path, path' = Queue.take queue in
    let x, y = List.hd path in
    if (x,y) = dst
    then (cur_max_path := max_by_len path !cur_max_path ; Printf.printf "FOUND A PATH (%d) [#cur_max: %d]\n%!" (List.length path) (List.length !cur_max_path))
    else begin
      ugh prev_dir |> List.iter (fun dir ->
        let x, y = advance_pos x y dir in
        if valid_pos x y && not (PosHashtbl.mem visited (x,y)) && not (PosSet.mem (x, y) path') then begin
          PosHashtbl.add visited (x,y) () ;
          match map.(y).(x) with
          | Forest -> ()
          | _ -> Queue.add (dir, ((x,y)::path), PosSet.add (x,y) path') queue
        end
      )
    end
  done ;
  (* let paths = !paths in *)
  (* (paths |> List.sort (fun p1 p2 -> List.length p2 - List.length p1) |> List.hd |> List.length) - 1 *)
  (* let path = paths |> List.fold_left (fun acc path ->
    if List.length path > List.length acc then path else acc
  ) [] in *)
  let path = !cur_max_path in
  match path with [] -> failwith "impossible" | _ -> path |> List.length
;;



let () =
  let input = read_lines "input.txt" in
  let map = parse_map input in
  dump_map map true;
  (* map |> pt01 |> Printf.printf "pt01: %d\n%!" ; *)
  map |> pt02 |> Printf.printf "pt02: %d\n%!" ;
