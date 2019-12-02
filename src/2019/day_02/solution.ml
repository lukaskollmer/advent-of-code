let read_lines filename =
  let file = open_in filename in
  let rec imp acc = 
    try imp (input_line file :: acc)
    with End_of_file -> close_in file; List.rev acc
  in
  imp []


let set_nth n e =
  List.mapi (fun i x -> if i = n then e else x)


type opcode = Add | Mul | Halt

let opcode_from_int = function
  | 1 -> Add
  | 2 -> Mul
  | 99 -> Halt
  | _ -> failwith "invalid opcode"

let opcode_to_int = function
  | Add -> 1
  | Mul -> 2
  | Halt -> 99


let run program =
  let rec eval program = function
    | i::_ when i = opcode_to_int Halt -> program
    | i::op1::op2::op3::xs -> (
      let lhs = List.nth program op1 in
      let rhs = List.nth program op2 in
      let imp f = eval (program |> set_nth op3 (f lhs rhs)) xs in
      match opcode_from_int i with
      | Add -> imp ( + )
      | Mul -> imp ( * )
      | Halt -> program )
    | _ -> failwith "unknown input"
  in
  eval program program


let () =
  let input = read_lines "input.txt" |> List.hd |> String.split_on_char ',' |> List.map int_of_string in

  (* pt 1 *)
  input
  |> set_nth 1 12
  |> set_nth 2 2
  |> run
  |> List.hd
  |> Printf.printf "Part 1: %i\n" ;

  (* pt 2 *)
  let rec test a b =
    let program = input |> set_nth 1 a |> set_nth 2 b in
    match program |> run |> List.hd with
    | 19690720 -> a, b
    | _ ->
      if b < 99 then test a (b+1)
      else if a < 99 then test (a+1) 0
      else failwith "no input combonation found"
  in
  let (a, b) = test 0 0 in
  Printf.printf "Part 2: %i\n" (a*100 + b) ;
