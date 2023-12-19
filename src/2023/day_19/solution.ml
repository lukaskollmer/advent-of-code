module StringMap = Map.Make(String) ;;

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


let rec any f = function
  | [] -> false
  | x::xs -> f x || any f xs
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



let negate_cond = function
  | True -> failwith "ugh"
  | GT (p, v) -> LT (p, v + 1)
  | LT (p, v) -> GT (p, v - 1)
;;


let simplify_workflows workflows =
  let m = StringMap.of_list workflows in
  let rec imp acc_cur = function
    | [] -> failwith "unreachable"
    | [(cond, action)] -> begin
      assert (cond = True) ;
      match action with
        | Accept -> [acc_cur]
        | Reject -> []
        | SendToWorkflow w -> imp acc_cur (StringMap.find w m)
      end
    | (cond, action)::rules -> begin
      let path_if_cond_true = match action with
        | Accept -> [acc_cur @ [cond]]
        | Reject -> []
        | SendToWorkflow w -> imp (acc_cur @ [cond]) (StringMap.find w m)
      in
      let path_if_cond_false = imp (acc_cur @ [negate_cond cond]) rules in
      path_if_cond_true @ path_if_cond_false
      end
  in
  imp [] (StringMap.find "in" m)
;;


let simplify_conds conds =
  let rec imp p =
    let conds = List.filter (function True -> false | GT (p', _) | LT (p', _) -> p' = p) conds in
    conds |> List.fold_left (fun (lower, upper) -> function
      | GT (_, v) -> (max lower v, upper)
      | LT (_, v) -> (lower, min upper v)
      | _ -> failwith "ugh"
    ) (0, 4001)
  in
  (imp X, imp M, imp A, imp S)
;;


let pt01 conds parts =
  let aux x (lower, upper) =
    x > lower && x < upper
  in
  parts |> List.filter (fun (x,m,a,s) -> any (fun (x', m', a', s') ->
    aux x x' && aux m m' && aux a a' && aux s s'
  ) conds)
  |> List.fold_left (fun acc (x,m,a,s) -> acc + x+m+a+s) 0
;;


let pt02 conds parts =
  let aux (x,y) =
    y - (x + 1)
  in
  conds |> List.map (fun (x, m, a, s) ->
    aux x * aux m * aux a * aux s
  ) |> List.fold_left (+) 0
;;


let () =
  let workflows, parts = read_lines "input.txt" |> parse_input in
  let conds = workflows |> simplify_workflows |> List.map simplify_conds in
  pt01 conds parts |> Printf.printf "Pt01: %d\n%!" ;
  pt02 conds parts |> Printf.printf "Pt02: %d\n%!" ;
