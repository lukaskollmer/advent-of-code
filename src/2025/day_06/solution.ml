let read_lines filename =
  let file = open_in filename in
  let rec imp acc = 
    try imp (input_line file :: acc)
    with End_of_file -> close_in file; List.rev acc
  in
  imp []
;;


type op = Add | Mul ;;

type insts  = int list * op ;;
type insts' = insts list ;;

let op_of_string_opt = function
  | "+" -> Some Add
  | "*" -> Some Mul
  | _ -> None
;;

let char_of_op = function Add -> '+' | Mul -> '*' ;;


let pt01 input =
  let parse input =
    let num_cols = input |> List.hd |> String.split_on_char ' ' |> List.filter_map int_of_string_opt |> List.length in
    let rec imp (acc: insts') = function
      | [] -> acc
      | l::[] ->
        let ops = l |> String.split_on_char ' ' |> List.filter_map op_of_string_opt in
        List.combine acc ops |> List.map (fun ((n,_),o) -> (n,o))
      | l::ls ->
        let ns = l |> String.split_on_char ' ' |> List.filter_map int_of_string_opt in
        imp (List.combine ns acc |> List.map (fun (n,(ns,o)) -> (n::ns,o))) ls
    in
    imp (List.init num_cols (fun _ -> ([], Add))) input
  in
  let input = parse input in
  input |> List.fold_left (fun acc (nums, op) ->
    let (op, init) = match op with Add -> ( + ), 0 | Mul -> ( * ), 1 in
    acc + (nums |> List.fold_left op init)
  ) 0
;;

let drop_last l =
  let rec imp acc = function
    | [] | _::[] -> acc
    | x::xs -> imp (x::acc) xs
  in
  imp [] l
;;


let range a b =
  Seq.unfold (fun a -> if a >= b then None else Some (a,a+1)) a
;;

let pt02 input =
  let parse input =
    let w = input |> List.hd |> String.length in
    (* assert (input |> List.for_all (fun l -> String.length l = w)) ; *)
    let h = List.length input - 1 in
    let m = Array.init_matrix h w (fun x y -> (List.nth input x).[y]) in
    m
  in
  let m = parse input in
  let h = Array.length m in
  let w = Array.length (m.(0)) in
  let ops = List.nth input (List.length input - 1) |> String.split_on_char ' ' |> List.filter_map op_of_string_opt in
  let col_starts = List.nth input (List.length input - 1) |> String.to_seq |> List.of_seq
    |> List.mapi (fun i c -> if c <> ' ' then Some i else None)
    |> List.filter_map (fun x -> x)
  in
  List.combine ops (List.combine col_starts ((col_starts |> List.tl) @ [w])) |> List.fold_left (fun acc (op,(col,next_col)) ->
    let nums = range col next_col |> Seq.fold_left (fun acc x ->
      let num = range 0 h |> Seq.fold_left (fun acc y ->
        match m.(y).(x) with
          | ' ' -> acc
          | c -> let d = (int_of_char c) - 48 in match acc with None -> Some d | Some acc -> Some (acc * 10 + d)
      ) None in
      acc @ [num]
    ) [] |> List.filter_map (fun x -> x) in
    let (op,init) = match op with Add -> ( + ),0 | Mul -> ( * ),1 in
    acc + (nums |> List.fold_left op init)
  ) 0
;;


let () =
  let input = read_lines "input.txt" in
  input |> pt01 |> Printf.printf "Pt01: %i\n%!" ;
  input |> pt02 |> Printf.printf "Pt02: %i\n%!" ;
