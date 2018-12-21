let read_lines filename =
  let file = open_in filename in
  let rec imp acc = 
    try imp (input_line file :: acc)
    with End_of_file -> close_in file; List.rev acc
  in
  imp []

let get_chars s =
  let rec imp l = function
    | -1 -> l
    | i  -> imp (s.[i] :: l) (i-1)
  in
  imp [] (String.length s - 1)


let string_of_chars chars = 
  let buf = Buffer.create 16 in
  List.iter (Buffer.add_char buf) chars;
  Buffer.contents buf

let input = read_lines "input.txt" |> List.hd


type case = Lower | Upper


let char_get_case = function
  | c when c >= 'a' && c <= 'z' -> Lower
  | c when c >= 'A' && c <= 'Z' -> Upper
  | _ -> failwith "char_get_case"



let same_of_different_case a b =
  if a = b then false else
  let (a, b) = if a < b then a, b else b, a in
  a + 32 = b


let () =
  (* part 1 *)
  let rec imp acc flag = function
    | [] ->
      let contents = List.rev acc in
      if not flag then contents else imp [] false contents
    | x::[] -> imp (x::acc) flag []
    | curr :: (next :: rest) ->
      if same_of_different_case curr next then
        imp acc true rest
      else
        imp (curr :: acc) flag (next::rest)
  in
  Printf.printf "[part 1] #units: %i" (input |> get_chars |> List.map int_of_char |> imp [] false |> List.length) ;
