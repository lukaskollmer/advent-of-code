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


let repeat n l =
  let rec imp acc = function
    | 0 -> acc
    | n -> imp (acc @ l) (n-1)
  in imp [] n
;;

let repeat' n sep l =
  match n with
    | n when n <= 0 -> []
    | 1 -> l
    | n -> begin
      let rec imp acc = function
        | 0 -> acc
        (* | 1 -> acc @ l *)
        | n -> imp (acc @ [sep] @ l) (n-1)
      in imp l (n-1)
    end
;;


let drop_first n l =
  let rec imp = function
    | (0, xs) -> xs
    | (_, []) -> []
    | (n, x::xs) -> imp (n-1, xs)
  in imp (n, l)
;;

let take n l =
  let rec imp acc = function
    | (0, _) -> List.rev acc
    | (_, []) -> List.rev acc
    | (n, x::xs) -> imp (x::acc) (n-1, xs)
  in imp [] (n, l)
;;

let chunked n l =
  if n = 0 then [l] else
  let rec imp acc = function
    | [] -> List.rev acc
    | l -> imp ((take n l)::acc) (drop_first n l)
  in imp [] l
;;




type entry = Operational | Broken | Unknown ;;


let parse_input input =
  let parse_row row =
    let row = row |> String.split_on_char ' ' in
    let x = row |> List.hd |> get_chars |> List.map (function '.' -> Operational | '#' -> Broken | '?' -> Unknown) in
    let y = List.nth row 1 |> String.split_on_char ',' |> List.map int_of_string in
    (x, y)
  in
  input |> List.map parse_row
;;


let calc_row self = function
  | [], [] -> 1
  | [], _ -> 0
  | Operational::springs, counts -> self (springs, counts)
  | Broken::_, [] -> 0
  | Broken::Operational::springs, 1::counts -> self (Operational::springs, counts)
  | ((Broken::_) as springs), c::counts ->
      if List.length springs < c then 0
      else if springs |> take c |> List.exists ((=) Operational) then 0
      else if not (match List.nth_opt springs c with None -> true | Some x -> x <> Broken) then 0
      else self (drop_first (c+1) springs, counts)
  | Unknown::springs, counts ->
    (self (springs, counts)) + (self (Broken::springs, counts))
;;




module RowCalcHash = struct
  type t = (entry list * int list)
  let equal (a: t) (b: t): bool = a = b
  let hash (springs, counts) =
    let hash = List.fold_left (fun acc x -> acc lxor (match x with Operational -> 1 | Broken -> 2 | Unknown -> 3)) max_int springs in
    hash lxor List.fold_left (fun acc x -> acc lxor x) max_int counts
end

(* module RowCalcHashTbl = Hashtbl.Make(RowCalcHash) *)



let memo_rec f =
  let h = Hashtbl.create 16 in
  let rec g x =
    try Hashtbl.find h x
    with Not_found ->
      let y = f g x in
      Hashtbl.add h x y;
      y
  in
  g
;;


let run input =
  input
    |> chunked 100 (* we have 1000 entries and 10 cores --> 100 entries per core *)
    |> List.fold_left (fun acc chunk ->
      let calc_row = memo_rec calc_row in
      Domain.spawn (fun () ->
        chunk |> List.map calc_row |> List.fold_left (+) 0
      )::acc
    ) []
    |> List.map Domain.join
    |> List.fold_left (+) 0
;;


let pt01 input =
  input |> parse_input |> run
;;


let pt02 input =
  input
    |> parse_input
    |> List.map (fun (springs, counts) -> (repeat' 5 Unknown springs, repeat 5 counts))
    |> run
;;



let () =
  let input = read_lines "input.txt" in
  input |> pt01 |> Printf.printf "Pt01: %d\n%!" ;
  input |> pt02 |> Printf.printf "Pt02: %d\n%!" ;
