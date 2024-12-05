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


let rec pow a = function
  | 0 -> 1
  | 1 -> a
  | n -> 
    let b = pow a (n / 2) in
    b * b * (if n mod 2 = 0 then 1 else a)
;;

(* let make_number a1 a2 =
  (Char.code a1 - Char.code '0') * 10 + (Char.code a2 - Char.code '0')
;; *)

let make_int digits =
  digits |> List.rev |> List.mapi (fun i d -> d * (pow 10 i)) |> List.fold_left (+) 0
;;

let make_int' digits =
  digits
  |> List.map (fun c -> Char.code c - Char.code '0')
  |> make_int
;;



let rec drop_while p = function
  | [] -> []
  | x::xs -> if p x then drop_while p xs else x::xs
;;


let drop_while' p l =
  let rec imp acc = function
    | [] -> List.rev acc, []
    | x::xs -> if p x then imp (x::acc) xs else List.rev acc, xs
  in imp [] l
;;

let is_digit c = c >= '0' && c <= '9' ;;



let dump_rule (a,b) =
  Printf.printf "%i|%i\n%!" a b
;;

let dump_ordering l =
  l |> List.iter (fun page -> Printf.printf "%i,%!" page) ;
  Printf.printf "\n%!" ;
;;


let middle l =
  List.nth l (List.length l / 2)
;;


let parse_input input =
  let rec parse_rules acc = function
    | [] -> assert false
    | '\n'::'\n'::xs -> List.rev acc, xs
    | '\n'::xs -> parse_rules acc xs
    | a1::a2::'|'::b1::b2::xs ->
      parse_rules ((make_int' [a1; a2], make_int' [b1; b2])::acc) xs
    (* | xs -> Printf.printf "FAILED MATCH: '%s'\n%!" (xs |> List.to_seq |> String.of_seq) ; assert false *)
  in
  let rules, input = parse_rules [] input in
  let orderings = input |> List.to_seq |> String.of_seq |> String.split_on_char '\n' |> List.map (fun line ->
    line |> String.split_on_char ',' |> List.map get_chars |> List.map make_int'
  ) in
  rules |> List.iter dump_rule ;
  orderings |> List.iter dump_ordering ;
  rules, orderings
;;


let correct rules ordering =
  rules |> List.for_all (fun (x,y) ->
    match List.find_index ((=) x) ordering, List.find_index ((=) y) ordering with
      | None, _ | _, None -> true
      | Some x_idx, Some y_idx -> x_idx < y_idx
  )
;;

let pt01 rules orderings =
  orderings |> List.filter (correct rules) |> List.fold_left (fun acc ordering -> acc + (middle ordering)) 0
;;


let pt02 rules orderings =
  let cmp a b =
    if List.mem (a,b) rules then -1 else 1
  in
  orderings
  |> List.filter (fun o -> o |> correct rules |> not)
  |> List.map (List.sort cmp)
  |> List.fold_left (fun acc ordering -> acc + (middle ordering)) 0
;;


let () =
Printexc.record_backtrace true;;
  let input = read_lines "input.txt" in
  Printf.printf "#input: %i\n%!" (List.length input) ;
  let rules, orderings = input |> String.concat "\n" |> get_chars |> parse_input in
  pt01 rules orderings |> Printf.printf "pt01: %i\n%!" ;
  pt02 rules orderings |> Printf.printf "pt02: %i\n%!" ;
