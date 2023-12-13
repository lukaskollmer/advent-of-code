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
  let parse_line s = s |> get_chars |> List.map (function '.' -> Ash | '#' -> Rock) in
  input |> List.iter (fun l -> Printf.printf "LINE: '%s'\n" l) ;
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


let flip (x,y) = (y,x) ;;


(* let transpose l =
  let h = List.length l in
  let w = List.length (List.hd l) in
  (* let r = List.init w (fun x -> List.init h (fun y -> List.nth (List.nth l y) x))  in *)
  let r = List.init w (fun y ->
    List.init h (fun x ->
      List.nth (List.nth l x) y
    )
    |> List.rev
  )
  |> List.rev
  in
  assert (dims r = flip (dims l)) ;
  r
;; *)

let rotate l = l |> transpose |> List.map List.rev ;;



(* let parse_pattern axis input =
  let rows = input |> List.map get_chars |> List.map (List.map (function '.' -> Ash | '#' -> Rock)) in
  match axis with
    | Horizontal -> rows
    | Vertical -> transpose rows
;; *)



let zip a b =
  let rec imp acc = function
    | ([], _) | (_, []) -> List.rev acc
    | (x::xs, y::ys) -> imp ((x,y)::acc) (xs, ys)
  in imp [] (a, b)
;;


let drop n l =
  let rec imp = function
    | (0, xs) -> xs
    | (_, []) -> []
    | (n, x::xs) -> imp (n-1, xs)
  in imp (n, l)
;;

let drop_end n l =
  l |> List.rev |> drop n |> List.rev
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
    | l -> imp ((take n l)::acc) (drop n l)
  in imp [] l
;;




let string_of_row l = l |> List.map (function Ash -> "." | Rock -> "#") |> String.concat "" ;;

let dump_pattern p =
  let s = p |> List.map string_of_row |> String.concat "\n" in
  Printf.printf "%s\n%!" s ;
;;


let string_of_int_list l = "[" ^ (l |> List.map string_of_int |> String.concat ", ") ^ "]" ;;

let find_reflections' _ l =
  Printf.printf "\n\nPATTERN:\n%!" ;
  dump_pattern l;
  Printf.printf "\n\nfind_reflections' \n%!" ;
  Printf.printf "#rows: %d\n" (List.length l) ;
  Printf.printf "#cols: %d\n" (List.length (List.hd l)) ;
  let x = mk_range_excl 1 (List.length l) |> List.filter (fun row_idx ->
    Printf.printf "- row_idx: %d\n" row_idx ;
    let prev_indices = mk_range_excl 0 row_idx |> List.rev in
    let post_indices = mk_range_excl row_idx (List.length l) in
    Printf.printf "    looking at prev_indices = %s and post_indices = %s\n" (string_of_int_list prev_indices) (string_of_int_list post_indices) ;
    zip prev_indices post_indices |> List.for_all (fun (i, j) ->
      Printf.printf "    - i: %d, j: %d\n" i j ;
      Printf.printf "      i: %s\n" (string_of_row (List.nth l i)) ;
      Printf.printf "      j: %s\n" (string_of_row (List.nth l j)) ;
      let r = List.nth l i = List.nth l j in
      Printf.printf "      -> r: %b\n" r ;
      r
    )
  ) in
  (* if List.length x <> 1 then
    Printf.printf "x (#=%d): %s\n%!" (List.length x) (x |> List.map string_of_int |> String.concat ", ");
  assert (List.length x = 1) ;
  List.hd x *)
  (* if x = [] then
    begin
    Printf.printf "NO REFLECTIONS FOUND\n%!" ;
    dump_pattern l ;
    failwith "NO REFLECTIONS FOUND" ;
    end ; *)
  x
;;



let split_list i l =
  let rec imp acc = function
    | (0, xs) -> (List.rev acc, xs)
    | (_, []) -> (List.rev acc, [])
    | (n, x::xs) -> imp (x::acc) (n-1, xs)
  in imp [] (i, l)
;;

let find_reflections' m l =
  Printf.printf "\n\nfind_reflections'' %s\n%s\n%!" m (l |> List.map string_of_row |> String.concat "\n") ;
  let r = mk_range_excl 0 (List.length l)
    (* |> List.filter ((<>) 0) *)
    |> List.filter (fun i ->
      Printf.printf "  - filter(%d)\n" i ;
      let prev, post = split_list i l in
      Printf.printf "    [A]\n      prev: %s;\n      post: %s\n%!"
        (prev |> List.map string_of_row |> String.concat "; ")
        (post |> List.map string_of_row |> String.concat "; ") ;
      if prev = [] || post = []
      then false
      else begin
        let n = min (List.length prev) (List.length post) in
        let prev, post = (prev |> List.rev |> take n, post |> take n) in
        Printf.printf "    [B]\n      prev: %s;\n      post: %s\n%!"
          (prev |> List.map string_of_row |> String.concat "; ")
          (post |> List.map string_of_row |> String.concat "; ") ;
        prev = post
      end
    )
  in
  Printf.printf "-> r: %s\n" (string_of_int_list r) ;
  r
;;

let find_reflections l = 
  let h = find_reflections' "NORMAL" l in
  let v = find_reflections' "ROTATE" (rotate l) in
  if not (h <> [] || v <> []) then
    begin
    Printf.printf "NO REFLECTIONS FOUND\n%!" ;
    dump_pattern l ;
    failwith "NO REFLECTIONS FOUND" ;
    end ;
  (h, v)
;;


let inspect f l = List.iter f l ; l ;;
let inspecti f l = List.iteri f l ; l ;;


let pt01 patterns =
  patterns
    |> List.map (find_reflections)
    (* |> List.map (fun (h, v) -> (h |> List.map ((+) 1), v |> List.map ((+) 1))) *)
    |> inspecti (fun idx (h, v) -> Printf.printf "Pattern %d: (h: %s, v: %s)\n" idx (string_of_int_list h) (string_of_int_list v))
    |> List.map (fun (h, v) -> (List.fold_left (+) 0 h, List.fold_left (+) 0 v))
    (* |> List.mapi (fun i (h, v) ->
      (* let fmt l = l |> List.map string_of_int |> String.concat ", " in *)
      Printf.printf "#reflections in pattern %d: (h: %d, v: %d)\n" i h v;
      assert (not (h <> 0 && v <> 0)) ;
      (* assert (h <> 0 || v <> 0) ; *)
      (* match (h, v) with
        | ([], []) -> 0
        | ([h], [v]) -> v + 100 * h
        (* | ([h], []) -> 100 * h
        | ([], [v]) -> v *)
        | _ -> failwith "Unexpected number of reflections" *)
        v + 100 * h
    )
    |> List.fold_left (+) 0 *)
    |> List.fold_left (fun (h_acc, v_acc) (h, v) -> (h_acc + h, v_acc + v)) (0, 0)
    |> (fun (h,v) -> v + 100*h)
;;


let () =
  let input = read_lines "input.txt" in
  let patterns = input |> get_patterns in
  Printf.printf "#patterns: %d\n" (List.length patterns);
  patterns |> List.map List.length |> List.iter (Printf.printf "Pattern len: %d\n");
  patterns |> inspect (fun p -> Printf.printf "\n\n\nPATERNNNNN: \n%s\n\n" (p |> List.map string_of_row |> String.concat "\n"));
  patterns |> pt01 |> Printf.printf "Pt01: %d\n%!" ;