module IntSet = Set.Make(Int) ;;

let read_lines filename =
  let file = open_in filename in
  let rec imp acc = 
    try imp (input_line file :: acc)
    with End_of_file -> close_in file; List.rev acc
  in
  imp []
;;


let parse_input lines =
  let parse_range r =
    let cs = String.split_on_char '-' r in
    (List.nth cs 0 |> int_of_string, (List.nth cs 1 |> int_of_string) + 1)
  in
  let (fresh_ranges, available) = (
    let rec imp (f,a) = function
      | [] -> (List.rev f, List.rev a)
      | x1::""::x2::xs -> imp (x1::f, [x2]) xs
      | x::xs -> imp (if a = [] then (x::f, a) else (f, x::a)) xs
    in
    imp ([],[]) lines
  ) in
  let fresh_ranges = fresh_ranges |> List.map parse_range in
  let available = available |> List.map int_of_string in
  (fresh_ranges, available)
;;

let range a b =
  Seq.unfold (fun a -> if a >= b then None else Some (a,a+1)) a
;;


let pt01 input =
  let (f,a) = input in
  a |> List.fold_left (fun acc x ->
    acc + if List.exists (fun ((a,b): (int*int)) -> a <= x && x <= b) f then 1 else 0
  ) 0
;;


let overlap (r1: (int*int)) (r2: (int*int)) =
  let (r1l, r1u) = r1 in
  let (r2l, r2u) = r2 in
  r2l < r1u && r2u > r1l
;;


let overlap_or_adj r1 r2 =
  (overlap r1 r2) || (fst r2 = snd r1) || (fst r1 = snd r2)
;;


let union r1 r2 =
  let (r1l, r1u) = r1 in
  let (r2l, r2u) = r2 in
  (Int.min r1l r2l, Int.max r1u r2u)
;;


module RangeSet = struct
  module IntTuple = struct
    type t = int * int
    let compare (a1,a2) (b1,b2) =
      if Int.equal a1 b1 && Int.equal a2 b2 then 0 else if a1 < b1 then -1 else 1
  end
  
  module IntTupleSet = Set.Make(IntTuple) ;;
  
  type t = IntTupleSet ;;

  let empty = IntTupleSet.empty ;;

  let merge s =
    let rec imp s =
      let prev = s in
      let s = IntTupleSet.fold (fun r acc ->
        match IntTupleSet.find_first_opt (fun r' -> overlap_or_adj r' r) acc with
          | None -> IntTupleSet.add r acc
          | Some r' -> acc |> IntTupleSet.remove r' |> IntTupleSet.add (union r r')
      ) s IntTupleSet.empty in
      if IntTupleSet.equal s prev then s else imp s
    in
    imp s

  let insert r s =
    IntTupleSet.add r s |> merge

  let fold f acc s =
    IntTupleSet.fold (fun x acc -> f acc x) s acc
  
end


let pt02 (f,_) =
  let rec imp acc = function
    | [] -> acc
    | r::rs -> imp (RangeSet.insert r acc) rs
  in
  imp RangeSet.empty f |> RangeSet.fold (fun acc (a,b) -> acc + (b-a)) 0
;;


let () =
  let input = read_lines "input.txt" |> parse_input in
  input |> pt01 |> Printf.printf "Pt01: %i\n%!" ;
  input |> pt02 |> Printf.printf "Pt02: %i\n%!" ;
