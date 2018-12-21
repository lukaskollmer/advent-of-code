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


let same_of_different_case a b =
  if a = b then false else
  let (a, b) = if a < b then a, b else b, a in
  (int_of_char a) + 32 = (int_of_char b)

let rangei a b =
  let rec imp l i =
    if i < a then l else imp (i::l) (i-1)
  in
  imp [] b


let () =
  let input_chars = input |> get_chars in
  
  let rec perform_reactions acc flag = function
    | [] ->
      let contents = List.rev acc in
      if not flag then contents else perform_reactions [] false contents
    | x :: [] -> perform_reactions (x :: acc) flag []
    | curr :: (next :: rest) ->
      if same_of_different_case curr next then
        perform_reactions acc true rest
      else
        perform_reactions (curr :: acc) flag (next :: rest)
  in

  (* part 1 *)
  Printf.printf "[part 1] #units: %i\n" (input_chars |> perform_reactions [] false |> List.length) ;

  (* part 2 *)
  rangei (int_of_char 'a') (int_of_char 'z')
    |> List.map char_of_int
    |> List.map (fun c -> input_chars |> List.filter (fun c' -> Char.lowercase_ascii c' <> c))
    |> List.map (fun x -> perform_reactions [] false x)
    |> List.sort (fun a b -> (List.length a) - (List.length b))
    |> List.hd |> List.length
    |> Printf.printf "[part 2] #units: %i" ;