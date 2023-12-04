module IntSet = Set.Make(Int)

let read_lines filename =
  let file = open_in filename in
  let rec imp acc = 
    try imp (input_line file :: acc)
    with End_of_file -> close_in file; List.rev acc
  in
  imp []
;;


let inspect f l = List.iter f l ; l ;;


let parse_numbers input = 
  let input = List.nth (String.split_on_char ':' input) 1 in
  let winning, mine = (let split = String.split_on_char '|' input in (List.nth split 0, List.nth split 1)) in
  let imp string =
    string
      |> String.trim
      |> String.split_on_char ' '
      |> List.filter (fun s -> String.length s <> 0)
      (* |> inspect (fun x -> Printf.printf "%s\n" x) *)
      |> List.map int_of_string
    in
  (IntSet.of_list (imp winning), IntSet.of_list (imp mine))
;;


let rec calc_points = function
  | 0 -> 0
  | 1 -> 1
  | n -> 2 * calc_points (n-1)


let pt01 input =
  input
    |> List.map parse_numbers
    |> List.map (fun (winning, mine) -> IntSet.filter (fun x -> IntSet.mem x winning) mine |> IntSet.cardinal)
    |> List.map calc_points
    |> List.fold_left (+) 0
;;



let mk_range_incl l u =
  (* assert (u >= l) ; *)
  if not (u > l) then [] else
  let rec imp x =
    if x = l then [x] else x::imp (x-1)
  in
  List.rev (imp u)
;;

let mk_range_excl l u =
  (* assert (u > l) ; *)
  if not (u > l) then [] else
  let rec imp x =
    if x = l then [x] else x::imp (x-1)
  in
  List.rev (imp (u-1))
;;


let pt02 input = 
  let rounds: (IntSet.t * IntSet.t) list = List.map parse_numbers input in
  let arr = Array.make (List.length input) 1 in
  rounds |> List.iteri (fun idx (winning, mine) -> (
    (* Printf.printf "\n\nROUND %i\n" idx ; *)
    (* Array.iteri (fun idx num -> Printf.printf "arr.(%i) = %i\n" (idx+1) num) arr; *)
    let num_matches = IntSet.inter winning mine |> IntSet.cardinal in
    (* mk_range_excl (idx+1) (idx+1+num_matches) |> List.iter (fun idx' -> Printf.printf "idx': %i\n" idx') ; *)
    mk_range_excl (idx+1) (idx+1+num_matches) |> List.iter (fun idx' -> (
      assert (idx <> idx') ;
      if num_matches > 0 then
        arr.(idx') <- arr.(idx') + arr.(idx) ;
    ))
  ));
  (* Printf.printf "\n\nDONE:\n" ; *)
  (* Array.iteri (fun idx num -> Printf.printf "arr.(%i) = %i\n" (idx+1) num) arr; *)
  Array.fold_left (+) 0 arr
;;




let () =
  let input = read_lines "input.txt" in
  input |> pt01 |> Printf.printf "Pt01: %i\n" ;
  input |> pt02 |> Printf.printf "Pt02: %i\n" ;
