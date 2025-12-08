let read_lines filename =
  let file = open_in filename in
  let rec imp acc = 
    try imp (input_line file :: acc)
    with End_of_file -> close_in file; List.rev acc
  in
  imp []
;;


module Triple = struct
  type ('a) t = 'a * 'a * 'a
  let map f (a,b,c) = f a, f b, f c
  let to_string (a,b,c) = Printf.sprintf "(%i, %i, %i)" a b c ;;
  let fst (a,_,_) = a
end


module Coord = struct
  type t = int Triple.t
  let compare (a: t) (b: t) =
    let (a1,a2,a3) = a in
    let (b1,b2,b3) = b in
    match Int.compare a1 b1 with
      | 0 -> (
        match Int.compare a2 b2 with
          | 0 -> Int.compare a3 b3
          | x -> x
      )
      | x -> x
  let equal (a: t) (b: t) =
    let (a1,a2,a3) = a in
    let (b1,b2,b3) = b in
    Int.(equal a1 b1 && equal a2 b2 && equal a3 b3)
end

module CoordPair = struct
  type t = Coord.t * Coord.t
  let compare (a: t) (b: t) =
    let ((a1,a2,a3), (a4,a5,a6)) = a in
    let ((b1,b2,b3), (b4,b5,b6)) = b in
    match Int.compare a1 b1 with
      | 0 -> (
        match Int.compare a2 b2 with
          | 0 -> (
            match Int.compare a3 b3 with
              | 0 -> (
                match Int.compare a4 b4 with
                  | 0 -> (
                    match Int.compare a5 b5 with
                      | 0 -> Int.compare a6 b6
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
  lines |> List.map (fun l ->
    let nth = l |> String.split_on_char ',' |> List.nth in
    (nth 0 |> int_of_string, nth 1 |> int_of_string, nth 2 |> int_of_string)
  )
;;


let distance a b =
  let (a1,a2,a3) = a |> Triple.map float_of_int in
  let (b1,b2,b3) = b |> Triple.map float_of_int in
  sqrt ((Float.pow (a1 -. b1) 2.) +. (Float.pow (a2 -. b2) 2.) +. (Float.pow (a3 -. b3) 2.))
;;


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

let pt01 n1 n2 (input: int Triple.t list) =
  let input_seq = input |> List.to_seq in
  let combinations = Seq.product input_seq input_seq
    |> Seq.fold_left (fun (seen,out) (a,b) ->
      if ((a = b) || (CoordPairSet.((mem (a,b) seen) || (mem (b,a) seen))))
      then (seen,out)
      else ((CoordPairSet.add (a,b) seen), (Seq.cons (a,b) out))
    ) (CoordPairSet.empty, Seq.empty)
    |> snd
  in
  let sorted_by_distance = combinations
    |> List.of_seq
    |> List.sort (fun (a1,b1) (a2,b2) ->
      Float.compare (distance a1 b1) (distance a2 b2)
    )
    |> List.take n1
  in

  let circuits = sorted_by_distance
    |> List.fold_left (fun acc (a,b) ->
      acc |> CoordSetSet.add (CoordSet.singleton a) |> CoordSetSet.add (CoordSet.singleton b)
    ) CoordSetSet.empty
  in
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
  circuits
    |> CoordSetSet.to_list
    |> List.map CoordSet.cardinal
    |> List.sort Int.compare
    |> List.rev
    |> List.take n2
    |> List.fold_left ( * ) 1
;;




let pt02 (input: int Triple.t list) =
  let input_seq = input |> List.to_seq in
  let combinations = Seq.product input_seq input_seq
    |> Seq.fold_left (fun (seen,out) (a,b) ->
      if ((a = b) || (CoordPairSet.((mem (a,b) seen) || (mem (b,a) seen))))
      then (seen,out)
      else ((CoordPairSet.add (a,b) seen), (Seq.cons (a,b) out))
    ) (CoordPairSet.empty, Seq.empty)
    |> snd
  in
  let sorted_by_distance = combinations
    |> List.of_seq
    |> List.sort (fun (a1,b1) (a2,b2) ->
      Float.compare (distance a1 b1) (distance a2 b2)
    )
  in

  let circuits = sorted_by_distance
    |> List.fold_left (fun acc (a,b) ->
      acc |> CoordSetSet.add (CoordSet.singleton a) |> CoordSetSet.add (CoordSet.singleton b)
    ) CoordSetSet.empty
  in
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
      let circuits = CoordSetSet.add new_circuit circuits in
      if CoordSetSet.cardinal circuits = 1 then (
        Printf.printf "SINGLE CIRCUIT!!! %s; %s\n%!" (Triple.to_string a) (Triple.to_string b) ;
        Printf.printf "%i\n%!" (Triple.fst a * Triple.fst b) ;
        failwith "ITS SO OVER" ;
      ) ;
      circuits
  ) circuits in
  Printf.printf "#circuits: %i\n%!" (CoordSetSet.cardinal circuits) ;
;;


let () =
  let input = read_lines "input.txt" in
  input |> parse_input |> pt01 1000 3 |> Printf.printf "Pt01: %i\n%!" ;
  (* input |> parse_input |> pt02 |> Printf.printf "Pt02: %i\n%!" ; *)
  input |> parse_input |> pt02
