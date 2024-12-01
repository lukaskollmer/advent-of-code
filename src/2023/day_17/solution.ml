(* #load "unix.cma";; *)

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


let parse_input input =
  input
    |> List.map (fun s -> s |> get_chars |> List.map (fun c -> int_of_char c - 48) |> Array.of_list)
    |> Array.of_list
;;



let take n l =
  let rec imp acc = function
    | (0, _) -> List.rev acc
    | (_, []) -> List.rev acc
    | (n, x::xs) -> imp (x::acc) (n-1, xs)
  in imp [] (n, l)
;;



let path_cost grid path =
  path |> List.tl |> List.fold_left (fun acc (x,y) -> acc + grid.(y).(x)) 0
;;


let rec check_no_four_conseq_blocks = function
  | [] -> true
  | [_] -> true
  | [_;_] -> true
  | [_;_;_] -> true
  | (x, y) :: (x', y') :: (x'', y'') :: (x''', y''') :: rest ->
    if x = x' && x' = x'' && x'' = x''' then false
    else if y = y' && y' = y'' && y'' = y''' then false
    else check_no_four_conseq_blocks ((x', y') :: (x'', y'') :: (x''', y''') :: rest)
;;


type dir = Left | Right | Up | Down ;;
let all_dirs = [Left; Right; Up; Down] ;;

module StepSet = Set.Make(struct
  type t = dir * int * int
  let compare = compare
end) ;;



let advance_pos' n x y = function
  | Left  -> (x-n, y)
  | Right -> (x+n, y)
  | Up    -> (x, y-n)
  | Down  -> (x, y+n)
;;

let advance_pos = advance_pos' 1 ;;

let dims grid = (Array.length grid, Array.length grid.(0)) ;;

let dir_from_to (x, y) (x', y') =
  if x = x' then
    if y < y' then Down else Up
  else if y = y' then
    if x < x' then Right else Left
  else failwith "dir_from_to: not aligned"
;;

let dump_path_in_grid grid path =
  Array.iteri (fun y row ->
    Array.iteri (fun x _ ->
      if List.mem (x, y) path then Printf.printf "X"
      (* else Printf.printf "%d" grid.(y).(x) *)
      else Printf.printf "."
    ) row;
    Printf.printf "\n%!" ;
  ) grid ;
  Printf.printf "\n\n%!" ;
  Unix.sleepf 0.05 ; ()
;;


type point = int * int ;;


let find_paths grid (x_src, y_src) dst =
  let h, w = dims grid in
  let pos_valid x y = x >= 0 && x < w && y >= 0 && y < h in
  let distance_to_dst x y = abs (x - fst dst) + abs (y - snd dst) in
  let rec imp seen path =
    (* dump_path_in_grid grid path ; *)
    assert (path <> []) ;
    let x, y = List.hd path in
    if (x, y) = dst then (Printf.printf "FOUND A PATH\n" ; (seen, [path]))
    else
      let next_pos = all_dirs
        |> List.map (fun dir -> let x, y = advance_pos x y dir in dir, x, y)
        |> List.filter (fun (dir, x, y) -> (false || not (StepSet.mem (dir, x, y) seen)) && pos_valid x y)
        |> List.filter (fun (_, x, y) -> check_no_four_conseq_blocks (take 4 ((x,y)::path)))
        |> List.filter (fun (_, x, y) -> List.mem (x,y) path |> not)
        |> List.sort (fun (_, x, y) (_, x', y') -> compare (distance_to_dst x y) (distance_to_dst x' y'))
      in
      (* Printf.printf "#next_pos: %d\n%!" (List.length next_pos) ; *)
      let x = next_pos |> List.fold_left (fun (seen, paths) (dir, x, y) ->
          match imp (if (x,y) = dst then seen else StepSet.add (dir,x,y) seen) ((x,y)::path) with
          | seen, [] -> (seen, paths)
          | seen, paths' -> Printf.printf "#paths gotten from rec call: %d\n%!" (List.length paths') ; (seen, paths @ paths')
      ) (seen, []) in x
  in
  let _, r = imp (all_dirs |> List.map (fun d -> (d, x_src, y_src)) |> StepSet.of_list) [x_src, y_src] in
  assert (r <> []) ;
  r
      (* let next_pos = List.map (advance_pos x y) [Left; Right; Up; Down] in
      let next_pos = List.filter (fun (x, y) -> pos_valid x y && grid.(y).(x) = 0) next_pos in
      let next_pos = List.filter (fun p -> not (List.mem p path)) next_pos in
      List.concat (List.map (fun p -> imp (p :: path)) next_pos) *)
;;






module PriorityQueue = struct
  type priority = int
  type 'a queue = Empty | Node of priority * 'a * 'a queue * 'a queue
  let empty = Empty
  let rec insert queue prio elt =
    match queue with
      | Empty -> Node(prio, elt, Empty, Empty)
      | Node(p, e, left, right) ->
        if prio <= p
        then Node(prio, elt, insert right p e, left)
        else Node(p, e, insert right prio elt, left)
  exception Queue_is_empty
  let rec remove_top = function
    | Empty -> raise Queue_is_empty
    | Node(prio, elt, left, Empty) -> left
    | Node(prio, elt, Empty, right) -> right
    | Node(prio, elt, (Node(lprio, lelt, _, _) as left),
                      (Node(rprio, relt, _, _) as right)) ->
        if lprio <= rprio
        then Node(lprio, lelt, remove_top left, right)
        else Node(rprio, relt, left, remove_top right)
  let extract = function
    | Empty -> raise Queue_is_empty
    | Node(prio, elt, _, _) as queue -> (prio, elt, remove_top queue)
  let remove queue elt =
    let rec imp = function
      | Empty -> Empty
      | Node(prio, e, left, right) ->
        if e = elt then remove_top queue
        else Node(prio, e, imp left, imp right)
    in imp queue
  let update_priority queue prio elt =
    insert (remove queue elt) prio elt
end;;



module StringHash = Hashtbl.Make(String);;



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





let mk_range_excl a b =
  let rec imp acc = function
    | x when x = b -> List.rev acc
    | x -> imp (x :: acc) (x+1)
  in imp [] a
;;

let flat_map f l =
  let rec imp acc = function
    | [] -> acc
    | x::xs -> imp (f x @ acc) xs
  in imp [] l
;;


module PointHashtbl = Hashtbl.Make(
  struct type t = int * int
  let hash (x, y) = x lxor y
  let equal = (=)
end);;


(* let preprocess_graph grid =
  Printf.printf "preprocessing graph\n%!" ;
  let g = GraphHashtbl.create 16 in
  (* let visited = GraphHashtbl.create 16 in *)
  let weight_at (x,y) = grid.(y).(x) in
  let h, w = dims grid in
  let valid_pos (x, y) = x >= 0 && x < w && y >= 0 && y < h in
  mk_range_excl 0 h |> flat_map (fun y -> mk_range_excl 0 w |> List.map (fun x -> (x, y))) |> List.iter (fun (x, y) ->
    let reachable = all_dirs
    |> flat_map (fun d ->
      let p1 = advance_pos' 1 x y d in
      let p2 = advance_pos' 2 x y d in
      let p3 = advance_pos' 3 x y d in
      (* [(weight_at p1, p1); (weight_at p1 + weight_at p2, p2); (weight_at p1 + weight_at p2 + weight_at p3, p3)] *)
      assert (if not (valid_pos p1) then not (valid_pos p2) else true) ;
      assert (if not (valid_pos p2) then not (valid_pos p3) else true) ;
      [(p1, [p1]); (p2, [p1; p2]); (p3, [p1; p2; p3])] |> List.fold_left (fun acc (pos, steps) ->
        if not (valid_pos pos) then acc
        else (pos, (steps |> List.map (weight_at) |> List.fold_left (+) 0)) :: acc
      ) []
    )
    |> List.filter (fun (pos, _) -> valid_pos pos)
  in
    GraphHashtbl.add g (x, y) reachable ;
  ) ;
  Printf.printf "did preprocess graph\n%!" ;
  g
;; *)



let flip_dir = function Left -> Right | Right -> Left | Up -> Down | Down -> Up ;;


module IntHashtbl = Hashtbl.Make(Int) ;;



(* let update_ht (module H: Hashtbl.S) ht (k: H.key) f =
  match H.find_opt ht k with
  | None -> H.add ht k (f ())
  | Some v -> H.replace ht k (f v *)


(* Preprocesses the graph to make it compatible with A*.
   The way this works is that we encode the "max 3 steps in the same direction" rule
   directly into the graph, by adding extra nodes and edges for these cases.
   Returns a hashtable representing the processed graph, with the values representing 
   *)
let preprocess_graph (grid: int array array): (((int * int * int) list PointHashtbl.t) * ((int * int) IntHashtbl.t)) =
  Printf.printf "preprocessing graph\n%!" ;
  let module VisitedHashtbl = Hashtbl.Make(
    struct type t = int * int * int * dir
    let hash (x, y, s, d) = x lxor y lxor s lxor (match d with Left -> 0 | Right -> 1 | Up -> 2 | Down -> 3)
    let equal = (=)
  end) in
  let h, w = dims grid in
  let g: (int*int) list IntHashtbl.t = IntHashtbl.create 16 in
  let mapping_old_to_new: (int PointHashtbl.t) = PointHashtbl.create 16 in
  let mapping_new_to_old: ((int*int) IntHashtbl.t) = IntHashtbl.create 16 in
  (* let visited = GraphHashtbl.create 16 in *)
  let visited: (int*int) VisitedHashtbl.t = VisitedHashtbl.create (h * w) in
  let weight_at (x,y) = grid.(y).(x) in
  let valid_pos (x, y) = x >= 0 && x < w && y >= 0 && y < h in

  let node_idx = ref 0 in
  let make_node () =
    let idx = !node_idx in
    node_idx := !node_idx + 1 ;
    idx
  in

  let rec imp ((x,y) as cur) num_steps cur_dir =
    assert (num_steps >= 0 && num_steps < 4) ;
    assert (valid_pos cur) ;
    match VisitedHashtbl.find_opt visited (x, y, num_steps, cur_dir) with Some (weight, idx) -> (weight, idx) | None -> begin
      let node_idx = make_node () in
      VisitedHashtbl.add visited (x, y, num_steps, cur_dir) (weight_at cur, node_idx) ;
      all_dirs |> List.filter ((<>) (flip_dir cur_dir)) |> List.iter (fun dir ->
        let new_pos = advance_pos' 1 x y dir in
      );
      IntHashtbl.
      IntHashtbl.add g node_idx (weight_at cur) ;
    end
  in imp (0,0) 0 Right ;

  Printf.printf "did preprocess graph\n%!" ;
  g
;;





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

    let open_add node = PriorityQueue2.insert _fmt_elem open_set node.fscore node in
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
              (*let tentative_gscore = current.gscore + 1 in (*TODO add the grid cost here?*)*)
              (*let tentative_gscore = current.gscore + cost neighbor (reconstruct_path current []) in (*TODO add the grid cost here?*)*)
              let tentative_gscore = current.gscore + cost current.data neighbor in
              let neighbor_node =
                (* match PriorityQueue2.find open_set neighbor with *)
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




(* let astar_example =
  let heuristic_cost_estimate _ _ = 0 in
  let neighbors_from node =
    match node with
      | "A" -> ["B"; "C"]
      | "B" -> ["D"; "E"]
      | _ -> []
  in
  let start: string AStar.node = { data = "A"; gscore = max_int; fscore = max_int; parent = None } in
  let goal = "E" in
  match AStar.a_star_search start goal neighbors_from heuristic_cost_estimate with
    | None -> print_endline "Goal not reachable"
    | Some path -> List.iter (Printf.printf "%s -> ") path
  ;
  Printf.printf "\n%!";
;;



failwith "TODO" ;; *)











let dijkstra grid src =
  let h, w = dims grid in
  let distances = Array.make_matrix h w 0 in
  let prev = Array.make_matrix h w None in
  let q = ref PriorityQueue.empty in
  Array.iteri (fun y row -> Array.iteri (fun x _ ->
    if (x,y) <> src then
      distances.(y).(x) <- max_int ;
    q := PriorityQueue.insert !q (distances.(y).(x)) (x, y);
  ) row ) grid ;
  while (!q) <> PriorityQueue.empty do
    Printf.printf "STEP\n%!";
    let _, (x, y), q' = PriorityQueue.extract !q in
    q := q' ;
    let neighbours = all_dirs
      |> List.map (advance_pos x y)
      |> List.filter (fun (x, y) -> x >= 0 && x < w && y >= 0 && y < h)
    in
    List.iter (fun (x', y') ->
      let alt = distances.(y).(x) + grid.(y).(x) in
      if alt < distances.(y').(x') then
        Printf.printf "DECREASING PRIO (for %d, %d) from %d to %d\n%!" x' y' distances.(y').(x') alt ;
        distances.(y').(x') <- alt ;
        prev.(y').(x') <- Some (x, y) ;
        (* q := PriorityQueue.insert !q alt (x', y') *)
        q := PriorityQueue.update_priority !q alt (x', y')
    ) neighbours
  done ;
  (distances, prev)
;;


let dijkstra2 grid src =
  let h, w = dims grid in
  let distances = Array.make_matrix h w 0 in
  let prev = Array.make_matrix h w None in
  (* prev.(snd src).(fst src) <- Some 0 ; *)
  let q = PriorityQueue2.empty () in
  let _f (x,y) = Printf.sprintf "(%d, %d)" x y in
  (* PriorityQueue2.insert _f q 0 src ; *)
  Array.iteri (fun y row -> Array.iteri (fun x _ ->
    if (x,y) <> src then
      distances.(y).(x) <- max_int ;
    Printf.printf "\n\n\nINSERT (%d, %d) (prio: %d)\n%!" x y distances.(y).(x);
    let size_before = PriorityQueue2.size q in
    PriorityQueue2.insert _f q (distances.(y).(x)) (x, y);
    let size_after = PriorityQueue2.size q in
    assert (size_after = size_before + 1);
  ) row ) grid ;
  Printf.printf "INSERT DONE (#queue: %d)\n%!" (PriorityQueue2.size q) ;
  (* failwith "WTF" ; *)
  while not (PriorityQueue2.is_empty q) do
    Printf.printf "STEP (#queue: %d)\n%!" (PriorityQueue2.size q);
    (* q |> PriorityQueue2.to_list_bad |> List.iter (fun (x, y) -> Printf.printf "- (%d, %d)\n%!" x y) ; *)
    let _, (x, y) = PriorityQueue2.extract q in
    let neighbours = all_dirs
      |> List.map (advance_pos x y)
      |> List.filter (fun (x, y) -> x >= 0 && x < w && y >= 0 && y < h)
    in
    List.iter (fun (x', y') ->
      let alt = distances.(y).(x) + grid.(y').(x') in
      if alt < distances.(y').(x') then
        Printf.printf "DECREASING PRIO (for %d, %d) from %d to %d\n%!" x' y' distances.(y').(x') alt ;
        distances.(y').(x') <- alt ;
        prev.(y').(x') <- Some (x, y) ;
        (* q := PriorityQueue.insert !q alt (x', y') *)
        PriorityQueue2.update_priority _f q alt (x', y')
        (* if PriorityQueue2.mem q (x',y') then
          PriorityQueue2.update_priority _f q alt (x', y')
        else
          PriorityQueue2.insert _f q alt (x', y') *)
    ) neighbours
  done ;
  let prev = prev |> Array.map (Array.map (function Some x -> x | None -> failwith "WTF")) in
  (distances, prev)
;;



let get_path_in_dijkstra src dst prevs =
  let rec imp acc cur = assert (not (List.mem cur acc)) ; match cur with 
    | (x, y) when (x, y) = src ->
      Printf.printf "branch 1 (%d, %d)\n%!" x y ;
      acc
    | (x, y) ->
      Printf.printf "branch 2 (%d, %d)\n%!" x y ;
      imp ((x, y) :: acc) prevs.(y).(x)
  in imp [] dst
;;


let pt01 grid =
  let paths = find_paths grid (0, 0) ((Array.length grid) - 1, (Array.length grid.(0)) - 1)
    |> List.filter (check_no_four_conseq_blocks)
    |> List.map List.rev
    |> List.sort (fun a b -> path_cost grid a - path_cost grid b)
    |> (fun x ->
      assert (x <> []);
      assert (x |> List.for_all ((<>) []));
      x)
    (* |> List.hd *)
  in
  Printf.printf "[pt01] #paths: %d\n%!" (List.length paths) ;
  Printf.printf "[pt01] path_costs:\n%!" ;
  paths |> List.iter (fun path -> Printf.printf "[pt01] - %d\n%!" (path_cost grid path) ; dump_path_in_grid grid path) ;
  paths |> List.hd |> path_cost grid ;
  Printf.printf "Running the dijkstra step\n%!" ;
  let dist, prev = dijkstra2 grid (0,0) in
  Printf.printf "Dijkstra done\n%!" ;
  Printf.printf "dist[dst] = %d\n%!" dist.((Array.length grid) - 1).((Array.length grid.(0)) - 1) ;
  (* let path = dijkstra 0 (grid |> Array.map (Array.map float_of_int)) in
  path |> Array.iter (fun x -> Printf.printf "%f -> " x) ; *)
  Printf.printf "distances:\n%!" ;
  dist |> Array.iteri (fun y row ->
    row |> Array.iteri (fun x _ ->
      Printf.printf "% 3d" dist.(y).(x)
    ) ;
    Printf.printf "\n%!"
  ) ;
  Printf.printf "prev:\n%!" ;
  prev |> Array.iteri (fun y row ->
    row |> Array.iteri (fun x _ ->
      match prev.(y).(x) with
      | x, y -> Printf.printf "(%2d,%2d) " x y
    ) ;
    Printf.printf "\n%!"
  ) ;


  let astar_stuff = (
    let graph = preprocess_graph grid in
    let heuristic_cost_estimate (x,y) (x',y') path =
      Printf.printf "\n\nHEURISTIC CALLED (%d, %d) --> (%d, %d)\n%!" x y x' y';
      (* Printf.printf "path so far:\n%!" ;
      dump_path_in_grid grid path ; *)
      (* if not (check_no_four_conseq_blocks path) then max_int *)
      (* else abs (x - x') + abs (y - y') *)
      (* else 0 *)
      0
    in
    let cost (x,y) (x',y') =
      Printf.printf "COST CALLED (%d, %d) --> (%d, %d)\n%!" x y x' y';
      (* Printf.printf "path so far:\n%!" ;
      dump_path_in_grid grid path ; *)
      (* if not (check_no_four_conseq_blocks path) then max_int
      else grid.(y).(x) *)
      match GraphHashtbl.find_opt graph (x,y) with
        | None -> failwith "impossible" ; max_int
        | Some l -> List.assoc (x',y') l
    in
    let h, w = dims grid in
    let neighbors_from (x,y) =
      (* match node with
        | "A" -> ["B"; "C"]
        | "B" -> ["D"; "E"]
        | _ -> [] *)
        (* all_dirs |> List.map (advance_pos x y) |> List.filter (fun (x, y) -> x >= 0 && x < w && y >= 0 && y < h) *)
        match GraphHashtbl.find_opt graph (x,y) with
          | None -> []
          | Some l -> l |> List.map fst
      in
    let start: (int*int) AStar.node = { data = (0,0); gscore = max_int; fscore = max_int; parent = None } in
    let goal = (w-1, h-1) in
    let path = AStar.a_star_search start goal neighbors_from cost heuristic_cost_estimate in
    match path with
      | None -> print_endline "Goal not reachable"
      | Some path -> List.iter (fun (x,y) -> Printf.printf "(%d, %d) -> " x y) path
    ;
    Printf.printf "\n%!";
    dump_path_in_grid grid path ;
    Printf.printf "path is valid: %b\n%!" (check_no_four_conseq_blocks path) ;
  ) in


  Printf.printf "getting path\n%!" ;
  (* failwith "TODO" ; *)
  let path = get_path_in_dijkstra (0,0) ((Array.length grid) - 1, (Array.length grid.(0)) - 1) prev in
  Printf.printf "got path\n%!" ;
  dump_path_in_grid grid path ;
  12
;;



let () =
  let input = read_lines "input1.txt" in
  let grid = parse_input input in
  grid |> pt01 |> Printf.printf "pt01: %d\n";