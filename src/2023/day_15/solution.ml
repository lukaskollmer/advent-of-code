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

let take_while f l =
  let rec imp acc = function
    | [] -> List.rev acc
    | x::xs -> imp (if f x then x::acc else acc) xs
  in imp [] l
;;

let drop n l =
  let rec imp = function
    | (0, xs) -> xs
    | (_, []) -> []
    | (n, x::xs) -> imp (n-1, xs)
  in imp (n, l)
;;

let split_list f l =
  let prefix = take_while f l in
  (prefix, drop (List.length prefix) l)
;;

let hash s =
  let rec imp acc = function
    | [] -> acc
    | x::xs -> imp (acc |> (+) (int_of_char x) |> ( * ) 17 |> (fun x -> (mod) x 256)) xs
  in imp 0 (s |> get_chars)
;;

let pt01 input =
  input
    |> String.split_on_char ','
    |> List.map (fun x -> x |> String.trim |> hash)
    |> List.fold_left (+) 0
;;




type op = Remove | Add of int ;;


let is_letter c =
  let c = int_of_char c in
  (c >= (int_of_char 'a') && c <= (int_of_char 'z')) || (c >= (int_of_char 'A') && c <= (int_of_char 'Z'))
;;


let string_of_chars cs =
  cs |> List.map (String.make 1) |> String.concat ""
;;

let parse_step s =
  let s = get_chars s in
  let (name, rest) = split_list is_letter s in
  let name = string_of_chars name in
  match rest with
    | ['-'] -> (name, Remove)
    | '='::rest ->
      let focal_length = rest |> string_of_chars |> int_of_string in
      (name, Add focal_length)
    | _ -> failwith "Invalid input"
;;



module IntMap = Map.Make(Int) ;;


let fold_lefti f acc l =
  let rec imp acc i = function
    | [] -> acc
    | x::xs -> imp (f acc i x) (i+1) xs
  in imp acc 0 l
;;

(* let fold_map (type k) (type t') (module M : Map.S with type key = k and type t = tt) f acc m =
  M.fold f m acc
;; *)

let pt02 input =
  let rec add_to_list n f = function
    | [] -> [n, f]
    | (n', _)::xs when n = n' -> (n, f) :: xs
    | x::xs -> x :: add_to_list n f xs
  in
  let update_map m name = function
    | Remove -> IntMap.update (hash name) (function
        | None -> None
        | Some lenses -> (match List.filter (fun (n, _) -> n <> name) lenses with [] -> None | xs -> Some xs)
      ) m
    | Add focal_length -> IntMap.update (hash name) (function
        | None -> Some [name, focal_length]
        | Some lenses -> Some (add_to_list name focal_length lenses)
      ) m
  in
  let buckets = input
    |> String.split_on_char ','
    |> List.map parse_step
    |> List.fold_left (fun acc (name, op) -> update_map acc name op) IntMap.empty
  in
  IntMap.fold (fun bucket lenses acc ->
      acc + fold_lefti (fun acc idx (_, focal_length) -> acc + ((bucket+1) * (idx+1) * focal_length)) 0 lenses
  ) buckets 0
;;


let () =
  let input = read_lines "input.txt" in
  assert (List.length input = 1) ;
  input |> List.hd |> pt01 |> Printf.printf "Pt01: %d\n%!" ;
  input |> List.hd |> pt02 |> Printf.printf "Pt02: %d\n%!" ;
