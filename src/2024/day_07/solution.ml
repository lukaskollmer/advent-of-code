module IntSet = Set.Make(Int) ;;

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


type op = Add | Mul | Concat ;;
type eq_elem = Val of int | Op of op ;;


let eq_elem_to_string = function
  | Val n -> string_of_int n
  | Op Add -> "+"
  | Op Mul -> "*"
  | Op Concat -> "||"
;;

let parse line =
  let [hd;tl] = line |> String.split_on_char ':' in
  let result = int_of_string hd in
  let tl = tl |> String.split_on_char ' ' |> List.filter ((<>) "") |> List.map (fun s -> Val (int_of_string s)) in
  result, tl
;;





let string_of_list l = l |> List.to_seq |> String.of_seq ;;



let rec op_perms = function
  | 0 -> []
  | 1 -> [[Op Add]; [Op Mul]]
  | n -> (op_perms (n-1)) |> List.concat_map (fun l -> [Op Add::l; Op Mul::l])
;;


let rec op_perms' = function
  | 0 -> []
  | 1 -> [[Op Add]; [Op Mul]; [Op Concat]]
  | n -> (op_perms' (n-1)) |> List.concat_map (fun l -> [Op Add::l; Op Mul::l; Op Concat::l])
;;


module IntHashTbl = Hashtbl.Make(Int) ;;


let memo_rec f =
  let h = Hashtbl.create 16 in
  let rec g x =
    try Hashtbl.find h x
    with Not_found ->
      let y = f g x in
      Hashtbl.add h x y;
      y
  in
  g
;;

(* let op_perms = memo_rec op_perms ;;
let op_perms' = memo_rec op_perms' ;; *)


let dump_eq eq =
  eq |> List.map eq_elem_to_string |> String.concat " " |> Printf.printf "Eq: %s\n%!"
;;


let digits n =
  let rec loop n acc =
    if n = 0 then acc
    else loop (n/10) (acc + 1)
  in
  match n with
    | 0 -> 1
    | _ -> loop n 0
;;


let rec pow a = function
  | 0 -> 1
  | 1 -> a
  | n -> 
    let b = pow a (n / 2) in
    b * b * (if n mod 2 = 0 then 1 else a)



let intersperse xs ys =
  let rec imp acc = function
    | xs, [] -> (List.rev acc) @ xs
    | [], ys -> (List.rev acc) @ ys
    | x::xs, y::ys -> imp (y::x::acc) (xs, ys)
  in
  imp [] (xs,ys)
;;



let solve res eq =
  let rec imp acc eq =
    if acc > res then false else match eq with
    | [] -> acc = res
    | Op Add :: Val n :: eq -> imp (acc + n) eq
    | Op Mul :: Val n :: eq -> imp (acc * n) eq
    | Op Concat :: Val n :: eq -> imp (acc * (pow 10 (digits n)) + n) eq
    | _ -> failwith "invalid eq format"
  in
  match eq with
    | Val n :: eq -> imp n eq
    | _ -> failwith "invalid eq format"
;;


let pt01 eqs =
  eqs |> List.fold_left (fun acc (res,eq) -> 
    let ops_perms = op_perms (List.length eq - 1) in
    let eqs = ops_perms |> List.map (intersperse eq) in
    let has_solution = eqs |> List.exists (fun eq -> solve res eq) in
    acc + if has_solution then res else 0
  ) 0
;;



let pt02 eqs =
  let op_perms = Array.init (eqs |> List.fold_left (fun acc (_,eq) -> max acc (List.length eq + 1)) 0) op_perms' in
  eqs |> List.fold_left (fun acc (res,eq) ->
    let ops_perms = op_perms.(List.length eq - 1) in
    let eqs = ops_perms |> List.map (intersperse eq) in
    let has_solution = eqs |> List.exists (fun eq -> solve res eq) in
    acc + if has_solution then res else 0
  ) 0
;;


let () =
Printexc.record_backtrace true ;
  let input = read_lines "input.txt" |> List.filter ((<>) "") in
  let eqs = input |> List.map parse in
  eqs |> pt01 |> Printf.printf "pt01: %i\n%!" ;
  eqs |> pt02 |> Printf.printf "pt02: %i\n%!" ;
