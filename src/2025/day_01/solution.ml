let read_lines filename =
  let file = open_in filename in
  let rec imp acc = 
    try imp (input_line file :: acc)
    with End_of_file -> close_in file; List.rev acc
  in
  imp []
;;

let count f =
  let rec imp acc = function
    | [] -> acc
    | x::xs -> imp (acc + if f x then 1 else 0) xs
  in
  imp 0
;;

type direction = Left | Right ;;

let parse_input lines =
  lines |> List.map (fun l ->
    match l |> String.to_seq |> Seq.uncons |> Option.get with
      | 'L', n -> (Left, n |> String.of_seq |> int_of_string)
      | 'R', n -> (Right, n |> String.of_seq |> int_of_string)
      | _ -> failwith "invalid input"
  )
;;

let div_mod a b = (a / b, a mod b) ;;


(* PT 01 *)

let rec adv_state state n = function
  | Right -> (state + n) mod 100
  | Left ->
    if n <= state then state - n
    else adv_state 99 (n - state - 1) Left
;;

let pt01 insts =
  let rec steps state acc = function
    | [] -> List.rev acc
    | (d,n)::insts -> steps (adv_state state n d) (state::acc) insts
  in
  insts |> steps 50 [] |> count (fun s -> s = 0)
;;


(* PT 02 *)

let rec adv_state' (state, num_at_0) n = function
  | Right -> let quotient, remainder = div_mod (state + n) 100 in (remainder, num_at_0 + quotient)
  | Left ->
    if n <= state then
      let state = state - n in (state, num_at_0 + if state = 0 then 1 else 0)
    else
      adv_state' (99, num_at_0 + if state <> 0 then 1 else 0) (n - state - 1) Left
;;

let pt02 insts =
  insts |> List.fold_left (fun acc (d,n) ->
    adv_state' acc n d
  ) (50, 0) |> snd
;;


let () = 
  let insts = read_lines "input.txt" |> parse_input in
  insts |> pt01 |> Printf.printf "Pt01: %i\n" ;
  insts |> pt02 |> Printf.printf "Pt02: %i\n" ;
