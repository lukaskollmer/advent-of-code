#use "topfind" ;;
#require "extlib" ;;


let input = open_in "input.txt" |> Std.input_list


let get_chars s =
  let rec imp l = function
    | -1 -> l
    | i  -> imp (s.[i] :: l) (i-1)
  in
  imp [] (String.length s - 1)

let range a b =
  let rec imp l i =
    if i < a then l else imp (i::l) (i-1) in
  imp [] (b-1)


let is_digit c = c >= '0' && c <= '9'

let parse_ints str =
  let rec imp ints curr_acc = function
    | [] -> ints @ [curr_acc]
    | x::xs ->
      if is_digit x then
        imp ints (curr_acc @ [x]) xs
      else
        imp (ints @ [curr_acc]) [] xs
  in
  imp [] [] (get_chars str)
  |> List.filter (fun l -> l <> [])
  |> List.map (fun digits ->
    digits |> List.map (fun c -> (int_of_char c) - (int_of_char '0'))
           |> List.fold_left (fun acc x -> acc * 10 + x) 0)

let parse_claim str =
  match parse_ints str with
  | [id; origin_x; origin_y; width; height] ->
    (id, (origin_x, origin_y), (width, height))
  | _ -> failwith "should never reach here"


(* returns width, height tuple *)
let get_claims_size claims = claims
  |> List.fold_left
    (fun (w, h) (_, (x, y), (w', h')) -> (max w (x + w'), max h (y + h')))
    (0, 0)


let rec process_claims arr = function
  | [] -> ()
  | x::xs ->
    let (_, (x, y), (width, height)) = x in
    for x' = x to x + width - 1 do
      for y' = y to y + height - 1 do
        arr.(x').(y') <- arr.(x').(y') + 1
      done;
    done;
    process_claims arr xs


let _ =
  (* prepare input *)
  let claims = input |> List.map parse_claim in
  
  let (width, height) = get_claims_size claims in
  let data = Array.init (width + 1) (fun _ -> Array.make (height + 1) 0) in

  process_claims data claims ;
  
  (* part 1 *)
  data
    |> Array.map (fun x -> x |> Array.to_list)
    |> Array.to_list
    |> List.flatten
    |> List.filter (fun x -> x > 1)
    |> List.length
    |> Printf.printf "part 1: %i\n" ;
  
  (* part 2 *)
  let rec first_where f = function
    | [] -> failwith "first_where"
    | x::xs -> if f x then x else first_where f xs
  in
  let rec all f = function [] -> true | x::xs -> if f x then all f xs else false in
  let field_equals value (x, y) arr = arr.(x).(y) = value in

  let (id, _, _) = claims
    |> first_where (fun (id, (x, y), (width, height)) ->
      range x (x+width) |> all (fun x -> range y (y+height) |> all (fun y -> field_equals 1 (x, y) data))
    )
  in

  Printf.printf "part 2: %i\n" id ;

