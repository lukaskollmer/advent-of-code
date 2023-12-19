(* #load "unix.cma";; *)

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


let get_opt = function Some x -> x | None -> failwith "None." ;;


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

let drop_end n l = take (List.length l - n) l ;;


let str_drop n s =
  if n < 0 then s
  else if n > String.length s then ""
  else String.sub s n (String.length s - n)
;;

let str_drop_end n s =
  if n < 0 then s
  else if n > String.length s then ""
  else String.sub s 0 (String.length s - n)
;;



type property = X | M | A | S ;;

type rule_cond = True | GT of property * int | LT of property * int ;;
type rule_action = Accept | Reject | SendToWorkflow of string ;;

type rule = rule_cond * rule_action ;;

let parse_rule = function
  | "A" -> True, Accept
  | "R" -> True, Reject
  | s ->
    if not (String.contains s ':')
    then (True, SendToWorkflow s)
    else begin
      (* Printf.printf "rule: %s\n%!" s ; *)
      let s = String.split_on_char ':' s in
      let p = match (List.hd s).[0] with 'x' -> X | 'm' -> M | 'a' -> A | 's' -> S | _ -> failwith "" in
      let v = int_of_string (List.hd s |> str_drop 2) in
      let cond = match (List.hd s).[1] with
        | '<' -> LT (p, v)
        | '>' -> GT (p, v)
        | _ -> failwith "ugh"
      in
      let action = match List.nth s 1 with
        | "A" -> Accept
        | "R" -> Reject
        | s -> SendToWorkflow s
      in
      (cond, action)
    end
;;

let parse_workflow s =
  let s = String.split_on_char '{' s in
  (* Printf.printf "workflow.split:\n%!" ; s |> List.iter (Printf.printf "- %s\n%!") ; *)
  let name = List.hd s in
  let rules = List.nth s 1 |> str_drop_end 1 |> String.split_on_char ',' |> List.map parse_rule in
  (name, rules)
;;

let parse_part s =
  let s = s |> str_drop 1 |> str_drop_end 1 in
  let comps = String.split_on_char ',' s |> List.map (str_drop 2) in
  let aux i = int_of_string (List.nth comps i) in
  (aux 0, aux 1, aux 2, aux 3)
;;


let parse_input input =
  let split_idx = input |> List.find_index ((=) "") |> get_opt in
  let workflows = input |> take split_idx |> List.map parse_workflow in
  let parts = input |> drop (split_idx+1) |> List.map parse_part in
  (workflows, parts)
;;



let get_property = function
  | X -> fun (x, _, _, _) -> x
  | M -> fun (_, m, _, _) -> m
  | A -> fun (_, _, a, _) -> a
  | S -> fun (_, _, _, s) -> s
;;


let eval_cond cond part =
  match cond with
    | True -> true
    | GT (p, v) -> (get_property p part) > v
    | LT (p, v) -> (get_property p part) < v
;;


module StringMap = Map.Make(String) ;;

let run workflows part =
  let m = StringMap.of_list workflows in
  (* let get n = match StringMap.find_opt n m with Some x -> x | None -> failwith (Printf.sprintf "no workflow named '%s'" n) in *)
  let rec imp = function
    | [] -> failwith "unreachable"
    | (cond, action)::rules ->
      if eval_cond cond part then begin
        match action with
          | Accept -> true
          | Reject -> false
          | SendToWorkflow w -> imp (StringMap.find w m)
        end
      else imp rules
  in
  imp (StringMap.find "in" m)
;;



let pt01 input =
  let workflows, parts = parse_input input in
  parts
    |> List.filter (run workflows)
    |> List.fold_left (fun acc (x,m,a,s) -> acc + x+m+a+s) 0
;;


let pt02 input =
  failwith "TODO" ;
;;


let () =
  let input = read_lines "input.txt" in
  input |> pt01 |> Printf.printf "Pt01: %d\n%!" ;
  input |> pt02 |> Printf.printf "Pt02: %d\n%!" ;