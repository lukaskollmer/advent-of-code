(* open Printf *)

let read_lines filename =
  let file = open_in filename in
  let rec imp acc = 
    try imp (input_line file :: acc)
    with End_of_file -> close_in file; List.rev acc
  in
  imp []
;;

let get_chars' s =
  let rec imp l = function
    | -1 -> l
    | i  -> imp ((i, s.[i]) :: l) (i-1)
  in
  imp [] (String.length s - 1)
;;


let rec num_digits = function
  | n when n < 0 -> failwith "negative"
  | n when n < 10 -> 1
  | n -> 1 + num_digits (n/10)
;;


let is_numeric_letter c = c >= '0' && c <= '9' ;;
let int_of_char' c = int_of_char c - int_of_char '0' ;;


let rec contains_where p = function
  | [] -> false
  | x::xs -> if p x then true else contains_where p xs
;;


type pos = {line: int; start: int; _end: int;}
type token_contents = Symbol of char | Number of int
type token = {contents: token_contents; pos: pos}


let is_symbol = function Symbol _ -> true | _ -> false ;;
let is_number = function Number _ -> true | _ -> false ;;


let print_tok ({contents; pos={line; start; _end}}) =
  Printf.printf "Token{contents=%s; pos={line=%i; start=%i; _end=%i}}\n"
    (match contents with Symbol _ -> "Symbol" | Number n -> string_of_int n)
    line start _end
;;


let tokenize (input: string list) : (token list) =
  let handle_line (line_no: int) (line: (int * char) list) : (token list) =
    (* failwith "TODO" *)
    let mk_tok: (int * token_contents) option -> token option = function
      | None -> None
      | Some (start, contents) -> Some {contents; pos={line=line_no; start; _end= start + (match contents with Symbol _ -> 1 | Number n -> num_digits n)}}
    in
    let mk_tok_append l cur =
      match mk_tok cur with None -> l | Some tok -> l @ [tok]
    in
    let rec imp (acc: token list) (cur: (int * token_contents) option) = function
      | [] ->
        mk_tok_append acc cur
      | (start, '.')::xs -> imp (mk_tok_append acc cur) None xs
      | (start, char)::xs when is_numeric_letter char -> (
        match cur with
          | None -> imp acc (Some (start, Number (int_of_char' char))) xs
          | Some (_, Symbol _) -> imp (mk_tok_append acc cur) (Some (start, Number (int_of_char' char))) xs
          | Some (start', Number n) -> imp acc (Some (start', Number (n*10+(int_of_char' char)))) xs
        )
      | (start, sym)::xs -> (* Not a period and not a digit -> must be a symbol *)
        imp (mk_tok_append acc cur) (Some (start, Symbol sym)) xs
    in
    imp [] None line
  in
  input
    |> List.mapi (fun line_no line -> handle_line line_no (get_chars' line))
    |> List.flatten
;;


let in_range_incl x a b = a <= x && x <= b ;;


let is_adjacent ({line; start; _end}) ({line=line'; start=start'; _end=_end'}) =
     (line = line' && (_end = start' || _end' = start))
  || (line = line' + 1 && (in_range_incl start (start'-1) _end' || in_range_incl start' (start-1) _end))
  || (line = line' - 1 && (in_range_incl start (start'-1) _end' || in_range_incl start' (start-1) _end))
;;


let rec find_adjacent pos' = function
  | [] -> []
  | ({contents;pos} as x)::xs -> if is_adjacent pos pos' then x::(find_adjacent pos' xs) else find_adjacent pos' xs
;;


let pt01 tokens =
  tokens
  |> List.filter_map
    (fun ({contents; pos}) ->
      match contents with
        | Symbol _ -> None
        | Number n -> if find_adjacent pos tokens |> contains_where (fun ({contents;pos=_}) -> is_symbol contents) then Some n else None
    )
  |> List.fold_left (+) 0
;;


let pt02 tokens =
  tokens
    |> List.filter_map
      (fun ({contents; pos}) ->
          match contents with
            | Symbol '*' ->
              let adjacent = find_adjacent pos tokens
                |> List.filter_map
                  (fun ({contents;pos=_}) ->
                    match contents with
                      | Number n -> Some n
                      | Symbol _ -> None
                  )
              in
              if List.length adjacent = 2 then Some (List.fold_left ( * ) 1 adjacent) else None
            | _ -> None
      )
    |> List.fold_left (+) 0
;;


let () =
  let input = read_lines "input.txt" |> tokenize in
  input |> pt01 |> Printf.printf "Pt01: %i\n" ;
  input |> pt02 |> Printf.printf "Pt02: %i\n" ;
;;