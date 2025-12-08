(* ocamlopt -g -O3 -I +unix unix.cmxa solution.ml *)

let read_lines filename =
  let file = open_in filename in
  let rec imp acc = 
    try imp (input_line file :: acc)
    with End_of_file -> close_in file; List.rev acc
  in
  imp []
;;


let measure l f =
  let start = Unix.gettimeofday () in
  let result = f () in
  let stop = Unix.gettimeofday () in
  Printf.printf "[MEASURE] %s took %f sec\n%!" l (stop -. start) ;
  result
;;

type cmp = Less | Equal | Greater ;;

let cmp_of_int x =
  if x = 0 then Equal 
  else if x < 0 then Less
  else Greater
;;


module Float = struct
  include Float
  let to_float x = x ;;
end

module Int = struct
  include Int
  let of_string = int_of_string
end

module Triple = struct
  type ('a) t = 'a * 'a * 'a
  let map f (a,b,c) = f a, f b, f c
  let fst (a,_,_) = a
end


module Coord = struct
  module Elt = Float ;;
  type t = Elt.t Triple.t
  let compare (a: t) (b: t) =
    let (a1,a2,a3) = a in
    let (b1,b2,b3) = b in
    match Elt.compare a1 b1 with
      | 0 -> (
        match Elt.compare a2 b2 with
          | 0 -> Elt.compare a3 b3
          | x -> x
      )
      | x -> x
  let equal (a: t) (b: t) =
    let (a1,a2,a3) = a in
    let (b1,b2,b3) = b in
    Elt.(equal a1 b1 && equal a2 b2 && equal a3 b3)
  
  let to_string (a,b,c) = Printf.sprintf "(%s, %s, %s)" (Elt.to_string a) (Elt.to_string b) (Elt.to_string c) ;;

  let of_string s =
    let nth = List.nth (String.split_on_char ',' s) in
    Elt.(of_string (nth 0), of_string (nth 1), of_string (nth 2))
end


module CoordPair = struct
  type t = Coord.t * Coord.t
  let equal (a: t) (b: t) =
    let ((a1,a2,a3), (a4,a5,a6)) = a in
    let ((b1,b2,b3), (b4,b5,b6)) = b in
    Coord.Elt.(equal a1 b1 && equal a2 b2 && equal a3 b3 && equal a4 b4 && equal a5 b5 && equal a6 b6)
  
    let hash (a: t) =
    let ((a1,a2,a3), (a4,a5,a6)) = a in
    Coord.Elt.(hash a1 lxor hash a2 lxor hash a3 lxor hash a4 lxor hash a5 lxor hash a6)
  
    let compare (a: t) (b: t) =
    let ((a1,a2,a3), (a4,a5,a6)) = a in
    let ((b1,b2,b3), (b4,b5,b6)) = b in
    let compare = Coord.Elt.compare in
    match compare a1 b1 with
      | 0 -> (
        match compare a2 b2 with
          | 0 -> (
            match compare a3 b3 with
              | 0 -> (
                match compare a4 b4 with
                  | 0 -> (
                    match compare a5 b5 with
                      | 0 -> compare a6 b6
                      | x -> x
                  )
                  | x -> x
              )
              | x -> x
          )
          | x -> x
      )
      | x -> x
end


module CoordSet = Set.Make(Coord) ;;
module CoordSetSet = Set.Make(CoordSet) ;;
module CoordPairSet = Set.Make(CoordPair) ;;


let parse_input lines =
  lines |> List.map Coord.of_string
;;


let distance (a,b) =
  let (a1,a2,a3) = a |> Triple.map Coord.Elt.to_float in
  let (b1,b2,b3) = b |> Triple.map Coord.Elt.to_float in
  sqrt ((Float.pow (a1 -. b1) 2.) +. (Float.pow (a2 -. b2) 2.) +. (Float.pow (a3 -. b3) 2.))
;;



let memo (f: CoordPair.t -> float) =
  let module H = Hashtbl.Make(CoordPair) in
  let h = H.create 11 in
  fun x ->
    try H.find h x
    with Not_found ->
      let y = f x in
      H.add h x y;
      y
;;
let distance = memo distance ;;
let distance a b = distance (a,b) ;;


let first_where (type s) (type elt) (module S : Set.S with type elt = elt and type t = s) (f: S.elt -> bool) (s: S.t) =
  S.fold (fun x acc ->
    match acc with
      | None -> if f x then Some x else None
      | Some x -> Some x
  ) s None
;;

let all_where (type s) (type elt) (module S : Set.S with type elt = elt and type t = s) (f: S.elt -> bool) (s: S.t): S.elt list =
  S.fold (fun x acc ->
    if f x then (x::acc) else acc
  ) s []
;;


let product_fold f acc a b =
  List.fold_left (fun acc a ->
    List.fold_left (fun acc b -> f acc a b) acc b
  ) acc a
;;



module Tree(Ord: Set.OrderedType) = struct
  type elt = Ord.t
  type t = Leaf | Node of t * elt * t

  let empty = Leaf

  let rec insert x t =
    match t with
      | Leaf -> Node (Leaf, x, Leaf)
      | Node (l,x',r) ->
        match Ord.compare x x' |> cmp_of_int with
          | Equal -> Node (l, x, r)
          | Less -> Node (insert x l, x', r)
          | Greater -> Node (l, x', insert x r)
  
  let rec inorder = function
    | Leaf -> []
    | Node (l,x,r) -> inorder l @ x :: inorder r
  
  let rec inorder_map f = function
    | Leaf -> []
    | Node (l,x,r) -> inorder_map f l @ f x :: inorder_map f r
    
  let rec fold f acc t =
    match t with
      | Leaf -> acc
      | Node (l,x,r) -> fold f (f x (fold f acc l)) r
end



let combinations input =
  let module T = Tree(struct
    type t = CoordPair.t * float
    let compare (p1, d1) (p2, d2) =
      match Float.compare d1 d2 with
        | 0 -> CoordPair.compare p1 p2
        | x -> x
  end) in
  product_fold (fun (seen,out) a b ->
    if a = b || CoordPairSet.(mem (a,b) seen || mem (b,a) seen) then seen,out
    else seen |> CoordPairSet.add (a,b), (T.insert ((a,b), (distance a b)) out)
  ) (CoordPairSet.empty, T.empty) input input |> snd |> T.inorder_map (fst)
;;

let preprocess sorted_by_distance limit =
  let sorted_by_distance = match limit with
    | None -> sorted_by_distance
    | Some limit -> List.take limit sorted_by_distance
  in
  let circuits = sorted_by_distance
    |> List.fold_left (fun acc (a,b) ->
      acc |> CoordSetSet.add (CoordSet.singleton a) |> CoordSetSet.add (CoordSet.singleton b)
    ) CoordSetSet.empty
  in
  (sorted_by_distance, circuits)
;;


let pt01 n1 n2 combinations =
  let (sorted_by_distance, circuits) = preprocess combinations (Some n1) in
  let circuits = sorted_by_distance |> List.fold_left (fun circuits (a,b) ->
    let matches = circuits |> all_where (module CoordSetSet) (fun circuit ->
      first_where (module CoordSet) (fun x -> Coord.(equal x a || equal x b)) circuit |> Option.is_some
    ) in
    if matches = [] then
      (* neither a nor b belongs to a circuit --> connect them and make them a new circuit *)
      CoordSetSet.add (a |> CoordSet.singleton |> CoordSet.add b) circuits
    else
      (* a and/or b belong to one or more circuits. we remove them from the set of circuits, merge them, add a and b, and add the result as a new circuit *)
      let circuits = CoordSetSet.filter (fun circuit -> not (List.mem circuit matches)) circuits in
      let new_circuit = List.fold_left (fun acc circuit -> CoordSet.union acc circuit) CoordSet.empty matches in
      let new_circuit = CoordSet.(new_circuit |> add a |> add b) in
      CoordSetSet.add new_circuit circuits
  ) circuits in
  CoordSetSet.fold (fun circuit acc -> (CoordSet.cardinal circuit)::acc) circuits []
    |> List.sort (fun a b -> -(Int.compare a b))
    |> List.take n2
    |> List.fold_left ( * ) 1
;;


let pt02 combinations =
  let (sorted_by_distance, circuits) = preprocess combinations None in
  let (_, result) = sorted_by_distance |> List.fold_left (fun (circuits,result) (a,b) ->
    match result with
      | Some _ -> (circuits, result)
      | None ->
        let matches = circuits |> all_where (module CoordSetSet) (fun circuit ->
          first_where (module CoordSet) (fun x -> Coord.(equal x a || equal x b)) circuit |> Option.is_some
        ) in
        if matches = [] then
          (* neither a nor b belongs to a circuit --> connect them and make them a new circuit *)
          (CoordSetSet.add (a |> CoordSet.singleton |> CoordSet.add b) circuits, None)
        else
          (* a and/or b belong to one or more circuits. we remove them from the set of circuits, merge them, add a and b, and add the result as a new circuit *)
          let circuits = CoordSetSet.filter (fun circuit -> not (List.mem circuit matches)) circuits in
          let new_circuit = List.fold_left (fun acc circuit -> CoordSet.union acc circuit) CoordSet.empty matches in
          let new_circuit = CoordSet.(new_circuit |> add a |> add b) in
          let circuits = CoordSetSet.add new_circuit circuits in
           if CoordSetSet.cardinal circuits = 1 then
            circuits, Some (Coord.Elt.mul (Triple.fst a) (Triple.fst b))
           else
            circuits, None
  ) (circuits, None) in
  result |> Option.get
;;


let () =
  let input = read_lines "input.txt" |> parse_input in
  let combinations = measure "combinations" (fun () -> combinations input) in
  measure "pt01" (fun () ->
    combinations |> pt01 1000 3 |> Printf.printf "Pt01: %i\n%!" );
  measure "pt02" (fun () ->
    combinations |> pt02 |> Coord.Elt.to_int |> Printf.printf "Pt02: %i\n%!" );
