let read_lines filename =
  let file = open_in filename in
  let rec imp acc = 
    try imp (input_line file :: acc)
    with End_of_file -> close_in file; List.rev acc
  in
  imp []


type step = int * int


let extract_steps input =
  input |> List.map (fun x ->
    let components = String.split_on_char ' ' x in
    let extract_char idx =
      String.get (List.nth components idx) 0 |> Char.code
    in
    (extract_char 1, extract_char 7)
  )


let () =
  let input = read_lines "input.txt" |> extract_steps in

  input |> List.iter (fun (prev, succ) ->
    Printf.sprintf "%i -> %i" prev succ |> print_endline ;
  ) ;

  exit 0