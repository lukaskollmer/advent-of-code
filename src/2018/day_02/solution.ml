#use "topfind" ;;
#require "extlib" ;;


let input = open_in "input.txt" |> Std.input_list

(* part 1 *)

let get_chars s =
  let rec imp l = function
    | -1 -> l
    | i  -> imp (s.[i] :: l) (i-1)
  in
  imp [] (String.length s - 1)

let count_of e l =
  let rec imp acc = function [] -> acc
    | x::xs ->
      if x = e then imp (acc+1) xs
      else imp acc xs
  in
  imp 0 l


let enumerated l = List.mapi (fun i e -> (i, e)) l
let get_counts l = List.map (fun e -> count_of e l, e) l

let num_with_count count input = input
  |> List.map get_chars
  |> List.filter (fun chars -> chars |> get_counts |> List.exists (fun (c, _) -> c = count))
  |> List.length

let num_twice = num_with_count 2 input
let num_thrice = num_with_count 3 input

let _ = num_twice * num_thrice |> Printf.sprintf "part 1: %i" |> print_endline



(* part 2 *)

let rec first_where f = function
  | [] -> None
  | x::xs -> if f x then Some x else first_where f xs

(* assuming that #a = #b *)
let differ_by_count a b c =
  let rec imp2 = function
    | count when count > c -> fun _ -> false
    | count -> function
      | 0 -> count = c
      | i ->
        if List.nth a i <> List.nth b i then
          imp2 (count+1) (i-1)
        else
          imp2 count (i-1)
  in
  imp2 0 (List.length a - 1)

let string_from_chars l = String.concat "" (List.map (String.make 1) l)

let input_as_chars = input |> List.map get_chars

let rec part2 = function
  | [] -> failwith ""
  | id::ids ->
    match input_as_chars |> first_where (fun i -> differ_by_count i id 1) with
    | None -> part2 ids
    | Some other_id -> (id, other_id)


let (id1, id2) = part2 input_as_chars
let _ = Printf.sprintf "\npart 2:\n- %s\n- %s" (string_from_chars id1) (string_from_chars id2) |> print_endline

let _ = id1
  |> enumerated
  |> List.filter (fun (i, c) -> List.nth id2 i = c)
  |> List.map (fun (_, c) -> c)
  |> string_from_chars |> Printf.printf "common letters: %s\n"