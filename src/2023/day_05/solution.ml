module IntSet = Set.Make(Int)
module IntMap = Map.Make(Int)
module CatMap = Map.Make(String)


let read_lines filename =
  let file = open_in filename in
  let rec imp acc = 
    try imp (input_line file :: acc)
    with End_of_file -> close_in file; List.rev acc
  in
  imp []
;;


let rec has_prefix p l = match p, l with
  | [], _ -> true
  | _::_, [] -> false
  | p::ps, l::ls -> if p = l then has_prefix ps ls else false
;;

let rec skip_first_n n l = match n, l with
  | _, [] -> []
  | 0, _ -> l
  | n, l::ls -> skip_first_n (n-1) ls
;;


let prefix_while p l =
  let rec imp acc = function
    | [] -> (acc, [])
    | x::xs -> if p x then imp (acc@[x]) xs else (acc, x::xs)
  in imp [] l
;;

let rec skip_while p = function
  | [] -> []
  | x::xs when p x -> skip_while p xs
  | xs -> xs
;;


let opt_get = function
  | Some a -> a
  | None -> failwith "None"
;;


let comp f g = fun x -> f (g x) ;;

let (^~) = comp ;;


let rec list_min = function
  | [] -> failwith "[]"
  | [x] -> x
  | x::xs -> min x (list_min xs)
;;


let last l = List.nth l (List.length l - 1) ;;


(* let split_list' pattern l =
  let rec imp acc cur = function
    | [] -> acc
    | l -> if has_prefix pattern l then imp (acc @ )
  in
;; *)



let parse_range line =
  let split = line |> String.split_on_char ' ' in
  if List.length split <> 3 then None
  else match List.map int_of_string_opt split with
    | [Some dst_start; Some src_start; Some len] -> Some (dst_start, src_start, len)
    | _ -> None
;;


let parse_input (parse_seeds: string -> IntSet.t) (input: string list) =
  let seeds = parse_seeds (List.hd input) in

  let input = skip_first_n 2 input in

  let rec parse_blocks acc l = match l with
    | [] -> acc
    | l::ls ->
      let name = String.split_on_char ' ' l |> List.hd in
      Printf.printf "NAME: %s\n" name ;
      let lines, rest = prefix_while (fun s -> s <> "") ls in
      let ranges = List.map (opt_get ^~ parse_range) lines in
      parse_blocks (CatMap.add name ranges acc) (skip_while (fun s -> s = "") rest)
  in

  let blocks = parse_blocks CatMap.empty input in

  (* let xxx = List.fold_left2 (fun acc line next -> (
    (* Printf.printf "\nline: %s\nnext: %s\n" line next ; *)
    []
  )) [] input ((List.tl input) @ [":EOL:"]) in
  [] *)
  (seeds, blocks)
;;


let rec map_number n = function
  | [] -> n (* if we've exhausted all ranges, and none matched the number, we apply the ideitity mapping *)
  | (dst_start, src_start, len)::rs ->
    if src_start <= n && (src_start+len) > n
    then dst_start + (n - src_start)
    else map_number n rs
;;

let rec seed_to_loc cats cur_cat (n: int) =
  match CatMap.filter (fun k _ -> String.starts_with ~prefix:cur_cat k) cats |> CatMap.choose_opt with
    | None -> n
    | Some (name, mapping) ->
      let n = map_number n mapping in
      match String.split_on_char '-' name |> last with
        | "location" -> n
        | next_cat -> seed_to_loc cats next_cat n
;;

let pt01 input =
  let seeds, cats = parse_input (fun str -> 
    str |> String.split_on_char ' ' |> List.filter_map int_of_string_opt |> IntSet.of_list
  ) input in
  seeds
    |> IntSet.map (seed_to_loc cats "seed")
    |> IntSet.min_elt
;;



let pt02 input =
  52
;;



let () =
  let input = read_lines "input.txt" in
  input |> pt01 |> Printf.printf "pt01: %i\n" ;
  input |> pt02 |> Printf.printf "pt02: %i\n" ;

