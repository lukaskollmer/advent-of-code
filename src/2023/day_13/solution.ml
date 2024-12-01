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


type tile = Ash | Rock ;;

let get_patterns input =
  let parse_line s = s |> get_chars |> List.map (function '.' -> Ash | '#' -> Rock | _ -> failwith "input") in
  let rec imp acc cur = function
    | [] -> List.rev (match cur with [] -> acc | cur -> (List.rev cur) :: acc)
    | ""::xs -> imp ((List.rev cur)::acc) [] xs
    | x::xs -> imp acc ((parse_line x)::cur) xs
  in
  imp [] [] input
;;

let mk_range_excl a b =
  if a >= b then [] else
  let rec imp acc i =
    if i = b then List.rev acc
    else imp (i::acc) (i+1)
  in
  imp [] a
;;


let rec flat_map f = function
  | [] -> []
  | x::xs -> f x @ flat_map f xs
;;


let sub l1 l2 =
  let rec imp n acc = function
    | ([], []) -> List.rev acc
    | ([], _) | (_, []) -> failwith "Invalid input"
    | (x::xs, y::ys) -> imp (n+1) (if x <> y then n::acc else acc) (xs, ys)
  in
  imp 0 [] (l1, l2)
;;



type axis = Horizontal | Vertical ;;


let rec transpose = function
  | [] -> []
  | []::xss -> transpose xss
  | (x::xs)::xss -> List.((x :: map hd xss) :: transpose (xs :: map tl xss))
;;


let dims l =
  let w = List.length l in
  let h = List.length (List.hd l) in
  (w, h)
;;


let rotate l = l |> transpose |> List.map List.rev ;;

let take n l =
  let rec imp acc = function
    | (0, _) -> List.rev acc
    | (_, []) -> List.rev acc
    | (n, x::xs) -> imp (x::acc) (n-1, xs)
  in imp [] (n, l)
;;




let split_list i l =
  let rec imp acc = function
    | (0, xs) -> (List.rev acc, xs)
    | (_, []) -> (List.rev acc, [])
    | (n, x::xs) -> imp (x::acc) (n-1, xs)
  in imp [] (i, l)
;;

let rec first_where f = function
  | [] -> None
  | x::xs -> if f x then Some x else first_where f xs
;;


let find_reflections num_impurities l = 
  let imp l =
    mk_range_excl 0 (List.length l) |> first_where (fun idx ->
      let prev, post = split_list idx l in
      if prev = [] || post = []
      then false
      else begin
        let n = min (List.length prev) (List.length post) in
        let prev, post = (prev |> List.rev |> take n, post |> take n) in
        List.combine prev post |> List.fold_left (fun acc (prev,post) -> acc + (sub prev post |> List.length)) 0 = num_impurities
      end
    )
  in
  let h = imp l in
  let v = imp (rotate l) in
  let aux = function Some i -> [i] | None -> [] in
  (aux h, aux v)
;;



let run num_impurities patterns =
  patterns
    |> List.map (find_reflections num_impurities)
    |> List.map (fun (h, v) -> (List.fold_left (+) 0 h, List.fold_left (+) 0 v))
    |> List.fold_left (fun (h_acc, v_acc) (h, v) -> (h_acc + h, v_acc + v)) (0, 0)
    |> (fun (h,v) -> v + 100*h)
;;


let pt01 = run 0 ;;
let pt02 = run 1 ;;


let () =
  let patterns = read_lines "input.txt" |> get_patterns in
  patterns |> pt01 |> Printf.printf "Pt01: %d\n%!" ;
  patterns |> pt02 |> Printf.printf "Pt02: %d\n%!" ;
