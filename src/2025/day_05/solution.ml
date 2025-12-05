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
    List.(nth cs 0 |> int_of_string, (nth cs 1 |> int_of_string) + 1)
  in
  let rec imp (f,a) = function
    | [] -> List.(f |> map parse_range, a |> map int_of_string)
    | x1::""::x2::xs -> imp (x1::f, [x2]) xs
    | x::xs -> imp (if a = [] then (x::f, a) else (f, x::a)) xs
  in
  imp ([],[]) lines
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


let union' r1 r2 =
  let (r1l, r1u) = r1 in
  let (r2l, r2u) = r2 in
  Int.(min r1l r2l, max r1u r2u)
;;


module RangeSet = struct
  module IntTuple = struct
    type t = int * int
    let compare (a1,a2) (b1,b2) =
      if Int.(equal a1 b1 && equal a2 b2) then 0 else if a1 < b1 then -1 else 1
  end
  
  module IntTupleSet = Set.Make(IntTuple) ;;
  
  type t = IntTupleSet ;;

  let empty = IntTupleSet.empty ;;

  let merge s =
    let open IntTupleSet in
    let rec imp s =
      let prev = s in
      let s = fold (fun r acc ->
        match find_first_opt (fun r' -> overlap_or_adj r' r) acc with
          | None -> add r acc
          | Some r' -> acc |> remove r' |> add (union' r r')
      ) s empty in
      if equal s prev then s else imp s
    in
    imp s

  let insert r s =
    IntTupleSet.add r s |> merge

  let fold f acc s =
    IntTupleSet.fold (fun x acc -> f acc x) s acc
  
end


let pt02 (f,_) =
  let open RangeSet in
  let rec imp acc = function
    | [] -> acc
    | r::rs -> imp (insert r acc) rs
  in
  imp empty f |> fold (fun acc (a,b) -> acc + (b-a)) 0
;;


let () =
  let input = read_lines "input.txt" |> parse_input in
  input |> pt01 |> Printf.printf "Pt01: %i\n%!" ;
  input |> pt02 |> Printf.printf "Pt02: %i\n%!" ;
