let read_lines filename =
  let file = open_in filename in
  let rec imp acc = 
    try imp (input_line file :: acc)
    with End_of_file -> close_in file; List.rev acc
  in
  imp []

let split_where f list =
  let rec imp accO accI = function
      [] -> accO @ (if accI <> [] then [List.rev accI] else [])
    | x::xs ->
        if f x then imp (accO @ (if accI <> [] then [List.rev accI] else [])) [] xs
        else imp accO (x::accI) xs
  in
  imp [] [] list

let parse raw_input =
  split_where ((=) "") raw_input
  |> List.map (List.map (int_of_string))


let rec sum = function
    [] -> 0
  | x::xs -> x + sum xs

type nat = Nil | Suc of nat

let rec nat_of_int = function
    n when n < 0 -> failwith "zero"
  | 0 -> Nil
  | n -> Suc (nat_of_int (n-1))


let rec take_n n l = match n with
    Nil -> []
  | (Suc n) -> match l with [] -> [] | (x::xs) -> x :: (take_n n xs)

let pt1 cals =
  cals
  |> List.map sum
  |> List.sort compare
  |> List.rev
  |> List.hd

let pt2 cals =
  cals
  |> List.map sum
  |> List.sort compare
  |> List.rev
  |> take_n (nat_of_int 3)
  |> sum


let () =
  let input = read_lines "input.txt" |> parse in
  (* List.iter (Printf.printf "line: %s\n") input *)
  input |> pt1 |> Printf.printf "Pt1 sol: %i\n" ;
  input |> pt2 |> Printf.printf "Pt2 sol: %i\n"
