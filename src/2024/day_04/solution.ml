(* 2644 *)

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
    | i  -> imp ((i, s.[i]) :: l) (i-1)
  in
  imp [] (String.length s - 1) |> List.map snd
;;


type elem = X | M | A | S | Other ;;

let elem_of_char = function
  | 'X' -> X
  | 'M' -> M
  | 'A' -> A
  | 'S' -> S
  | _ -> Other
;;



let count_where f =
  List.fold_left (fun acc x -> acc + if f x then 1 else 0) 0
;;


let rec last = function
  | [] -> assert false 
  | [x] -> x
  | x::xs -> last xs
;;

let rec init = function
  | [] -> []
  | [x] -> []
  | x::xs -> x::(init xs)
;;

let one_rot l = (last l)::(init l)
;;


let rec rot n l = 
  match n with 
  | 0 -> l
  | _ -> rot (n-1) (one_rot l)
;;


(* let shift n l = *)
  


let to_array lines =
  let len = List.length lines in
  (* let arr = Array.make_matrix len len  *)
  let arr = lines |> List.map (fun line -> Array.init len (fun i -> i |> List.nth line)) in
  arr |> Array.of_list
;;


let to_lists arr =
  let len = Array.length arr in
  List.init len (fun i -> arr.(i) |> Array.to_list)
;;

(* let array_rev arr = arr |> Array.to_list |> List.rev |> Array.of_list ;; *)

(* let matrix_rev arr = arr |> Array.map array_rev ;; *)

let range a b =
  let rec imp l i =
    if i < a then l else imp (i::l) (i-1) in
  imp [] (b-1)
;;

let zip x y =
  let rec imp = function
    | ([], _) | (_, []) -> []
    | (x::xs, y::ys) -> (x, y) :: imp (xs, ys)
  in
  imp (x, y)
;;


let rec transpose = function
  | [] -> []
  | []::xss -> transpose xss
  | (x::xs)::xss -> List.((x :: map hd xss) :: transpose (xs :: map tl xss))
;;


let rotate l = l |> transpose |> List.map List.rev ;;

let rotate = transpose ;;

(* let deep_copy arr = Array.init (Array.length arr) (fun i -> Array.copy arr.(i)) ;; *)


let extract_diags arr =
  let arr = to_array arr in
  let len = Array.length arr in
  let rows = (range 0 (len-0)) |> List.map (fun row_idx ->
    let r = range 0 (row_idx+1) in
    zip r (List.rev r) |> List.map (fun (i, j) -> arr.(i).(j))
    (* zip (range 0 (len-1)) (range 0 (len-1) |> List.rev) |> List.iter (fun (i, j) -> *)
  ) in
  assert (List.length rows = len) ;
  List.iteri (fun idx row -> assert (List.length row = idx+1)) rows;
  rows
  (* for row_idx = 0 to len-1 do
    (* 5;0  4;1  3;2  2;3  1;4  0;5 *)
    ()
  done; *)
;;


let take n l =
  let rec imp acc = function
    | (0, _) -> List.rev acc
    | (_, []) -> List.rev acc
    | (n, x::xs) -> imp (x::acc) (n-1, xs)
  in imp [] (n, l)
;;

let drop n l =
  let rec imp = function
    | (0, xs) -> xs
    | (_, []) -> []
    | (n, x::xs) -> imp (n-1, xs)
  in imp (n, l)
;;



let unshift_diags arr =
  let len = List.length arr in
  if true then List.mapi (fun idx line -> rot idx line) arr
  else
  let retval = arr |> List.mapi (fun idx line -> [
    (drop idx line) @ List.init idx (fun _ -> '.');
    (List.init idx (fun _ -> '.')) @ take (List.length line - idx) line]
    ) |> List.concat
  in
  retval |> List.iter (fun line -> assert (List.length line = List.length arr)) ;
  retval
;;

let filter_duplicates l =
  let rec imp seen = function
    | [] -> []
    | x::xs when List.mem x seen -> imp seen xs
    | x::xs -> x :: (imp (x::seen) xs)
  in imp [] l
;;



let extract_sequences arr =
  let arr = to_lists arr in
  Printf.printf "Will start constructing inputs\n%!" ;
  let inputs = [
    arr;
    arr |> rotate;
    (* arr |> rotate |> rotate; *)
    (* arr |> rotate |> rotate |> rotate *)
  ] in
  assert ((arr) = ((arr |> rotate |> rotate |> rotate |> rotate))) ;
  Printf.printf "#inputs: %i\n%!" (List.length inputs) ;
  let diag_shit = unshift_diags arr in
  let inputs = (inputs) @ [diag_shit; rotate diag_shit] in
  (* let inputs = filter_duplicates inputs in *)
  Printf.printf "#inputs: %i\n%!" (List.length inputs) ;
  inputs
;;


(* let has_head h l = take (List.length h) l = h ;; *)
let has_head h l =
  Printf.printf "has_head %s %s\n%!" (h |> List.to_seq |> String.of_seq) (l |> List.to_seq |> String.of_seq) ;
  if List.length h > List.length l then false else
  let rec eq = function
    | [] -> failwith "oh shit"
    | (x,x')::[] -> x = x'
    | (x,x')::xs -> x = x' && eq xs
  in
  let retval = eq (zip h l) in
  Printf.printf "has_head: %b \n%!" retval ;
  retval
;;




let count_occ e l =
  Printf.printf "count_occ %i %i\n%!" (List.length e) (List.length l) ;
  let rec imp acc = function
    | [] -> acc
    | xs -> if has_head e xs then imp (acc+1) (drop (List.length e) xs) else imp acc (List.tl xs)
  in
  let retval = imp 0 l in
  Printf.printf "count_occ DONE %i \n%!" retval ;
  retval
;;



let print_array l =
  l |> List.iter (fun line -> Printf.printf "%s\n%!" (line |> List.to_seq |> String.of_seq))
;;


let pt01 (lines: char list list) =
  lines |> to_array |> extract_sequences |> List.fold_left (fun acc (matrix: char list list) ->
    Printf.printf "\n\n\n\n\nMATRIX\n%!" ;
    print_array matrix ;
    (* let arr: char array array = to_array matrix in *)
    let idx = ref 0 in
    matrix |> List.fold_left (fun acc row ->
      Printf.printf "\nidx: %i\n%!" idx.contents ;
      Printf.printf "row: '%s'\n%!" (row |> List.to_seq |> String.of_seq) ;
      let nocc = (count_occ ['X';'M';'A';'S'] row) + (count_occ ['S';'A';'M';'X'] row) in
      Printf.printf "nocc: %i\n%!" nocc ;
      incr idx ;
      acc + nocc
    ) acc
  ) 0
;;


(* let diag_points_across ((x,y): (int*int)): ((int*int) list * (int*int) list) = *)
let diag_points_across (x,y) =
  [[(x-1,y-1); (x,y); (x+1,y+1)]; [(x-1,y+1); (x,y); (x+1,y-1)]]
;;

let nth' i l = List.nth l i ;;


let pt02 (lines: char list list) =
  let len = List.length lines in
  let get (x,y) = lines |> nth' y |> nth' x in
  let get' points = points |> List.map get in
  let is_mas (x,y) =
    diag_points_across (x,y) |> List.for_all (fun points -> get' points = ['M'; 'A'; 'S'] || get' points = ['S'; 'A'; 'M'])
  in
  range 1 (len-1) |> List.fold_left (fun acc x ->
    range 1 (len-1) |> List.fold_left (fun acc y ->
      acc + if is_mas (x,y) then 1 else 0
    ) acc
  ) 0
;;



let () =
  Printexc.record_backtrace true;;
  Printf.printf "STARTTTTT\n%!" ;
  let lines = read_lines "input.txt" |> List.map get_chars in
  assert ((List.length lines) = (lines |> List.map List.length |> List.fold_left max 0)) ;
  (* let lines2 = read_lines "input2.txt" |> List.map get_chars in
  lines2 |> unshift_diags |> print_array ; *)
  (* lines2 |> extract_diags |> List.iter (fun line -> Printf.printf "%s\n" (String.of_seq (List.to_seq line))) ;
  lines2 |> rotate |> extract_diags |> List.iter (fun line -> Printf.printf "%s\n" (String.of_seq (List.to_seq line))) ; *)
  lines |> pt01 |> Printf.printf "pt01: %i\n%!" ;
  (* lines |> pt02 |> Printf.printf "pt02: %i\n%!" ; *)
  (* assert (lines |> pt01 = 2644) ; *)