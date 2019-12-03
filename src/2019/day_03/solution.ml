let read_lines filename =
  let file = open_in filename in
  let rec imp acc = 
    try imp (input_line file :: acc)
    with End_of_file -> close_in file; List.rev acc
  in
  imp []


let manhattan (x, y) =
  abs x + abs y


type cmp = Eq | Lt | Gt

let cmp (x: int) (y: int): cmp =
  if x = y then Eq
  else if x < y then Lt
  else Gt


(* A Binary Tree data structure storing coordinates in a two-dimensional plane *)
module Tree = struct
  type t = int * int * int
  type tree = Leaf | Node of t * tree * tree

  let hash = Hashtbl.hash

  let compare_hash (x, y, _) (x', y', _) =
    cmp (hash (x, y)) (hash (x', y'))

  (* we can safely ignore the distance since we're interested in the shortest path, meaning that
  if the tree simply does not override existing points, it'll always store the smallest distance *)
  let compare (x, y, _) (x', y', _) =
    if x = x' && y = y' then Eq
    else cmp (manhattan (x, y)) (manhattan (x', y'))

  let rec insert v = function
    | Leaf -> Node (v, Leaf, Leaf)
    | Node (v', l, r) as node ->
      match compare_hash v v' with
      | Eq -> node
      | Lt -> Node (v', insert v l, r)
      | Gt -> Node (v', l, insert v r)
  
  let rec contains v = function
    | Leaf -> false
    | Node (v', l, r) ->
      match compare_hash v v' with
      | Eq ->
        (* only run a proper compare if the hash values are equal *)
        (match compare v v' with
        | Eq -> true
        | Lt -> contains v l
        | Gt -> contains v r)
      | Lt -> contains v l
      | Gt -> contains v r
  
  let rec first_where f = function
    | Leaf -> None
    | Node (v, l, r) ->
      match f v with
      | Eq -> Some v
      | Lt -> first_where f l
      | Gt -> first_where f r
  
  let rec to_list = function
    | Leaf -> []
    | Node (v, l, r) -> to_list l @ (v :: to_list r)
end



type direction = Up | Down | Left | Right

let direction_from_char = function
  | 'U' -> Up
  | 'D' -> Down
  | 'L' -> Left
  | 'R' -> Right
  | _ -> failwith "unknown input"


let get_move s =
  let dir = String.get s 0 in
  let len = String.sub s 1 (String.length s - 1) in
  (direction_from_char dir), (int_of_string len)


(* Returns the list of positions of the wire, relative to an origin (0, 0) in the bottom left corner *)
let parse_wire w =
  let rec walk ((x, y, d) as pos) path = function
    | [] -> path
    | (dir, 0)::xs -> walk pos path xs
    | (dir, n)::xs ->
      let imp f =
        let (x, y) = f (x, y) in
        let npos = (x, y, d+1) in
        walk npos (Tree.insert npos path) ((dir, n-1)::xs)
      in
      match dir with
      | Up    -> imp (fun (x, y) -> x, y+1)
      | Down  -> imp (fun (x, y) -> x, y-1)
      | Left  -> imp (fun (x, y) -> x-1, y)
      | Right -> imp (fun (x, y) -> x+1, y)
  in
  walk (0, 0, 0) Tree.Leaf (List.map get_move w)



let () =
  let input = read_lines "input.txt" |> List.map (String.split_on_char ',') in

  let w1 = parse_wire (List.nth input 0) in
  let w2 = parse_wire (List.nth input 1) in

  let intersections = w1 |> Tree.to_list |> List.filter (fun p -> Tree.contains p w2) in

  (* Part 1 *)
  intersections
  |> List.map (fun (x, y, _) -> manhattan (x, y))
  |> List.sort Int.compare
  |> List.hd
  |> Printf.printf "Part1: %i\n" ;

  (* Part 2 *)
  intersections
  |> List.filter_map (fun ((x, y, d) as p) ->
    match Tree.first_where (fun ((x', y', d') as p') -> if x = x' && y = y' then Eq else Tree.compare_hash p p') w2 with
    | None -> None
    | Some (x', y', d') -> Some (d + d') )
  |> List.sort Int.compare
  |> List.hd
  |> Printf.printf "Part2: %i\n" ;

  ()
