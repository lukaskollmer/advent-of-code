

let digits n =
  let rec loop n acc =
    if n = 0 then acc
    else loop (n/10) (acc + 1)
  in
  match n with
    | 0 -> 1
    | _ -> loop n 0
;;

let digits n =
  let rec loop acc = function
    | 0 -> acc
    | n -> loop (n mod 10 ::acc) (n / 10)
  in
  match n with
    | 0 -> [0]
    | _ -> loop [] n
;;


let split_half l =
  let n = List.length l / 2 in
  let rec imp acc i = function
    | [] -> List.rev acc, []
    | xs when i = n -> (List.rev acc), xs
    | x::xs -> imp (x::acc) (i+1) xs
  in
  imp [] 0 l
;;

let rec pow a = function
  | 0 -> 1
  | 1 -> a
  | n -> 
    let b = pow a (n / 2) in
    b * b * (if n mod 2 = 0 then 1 else a)
;;

let make_int digits =
  digits |> List.rev |> List.mapi (fun i d -> d * (pow 10 i)) |> List.fold_left (+) 0
;;


(* module Map = Hashtbl.Make(struct
  type t = int*int*int
  let compare = compare
end) *)

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

let rec step1 acc it n =
  if it <= 0 then acc else match n with
  | 0 -> step1 acc (it-1) 1
  | n ->
    let digits = digits n in
    if (List.length digits) mod 2 = 0 then
      let a,b = split_half digits in step1 acc (it-1) (make_int a) + step1 acc (it-1) (make_int b)
    else step1 acc (it-1) (n * 2024)
;;

(* let step1 = memo_rec step1 ;; *)

let step = function
  | 0 -> [1]
  |n ->
    let digits = digits n in
    if (List.length digits) mod 2 = 0 then
      let a,b = split_half digits in [make_int a; make_int b]
    else [n * 2024]
;;


let rec run state n =
  Printf.printf "run (#remaining: %i)\n%!" n ;
  (* state |> List.iter (fun n -> Printf.printf "%i %!" n) ;
  print_newline () ; *)
  if n <= 0 then state else
  run (state |> List.concat_map step) (n-1)
;;


let run2 state it =
  (* let step1 = memo_rec step1 in *)
  state
  |> List.map (fun n -> Domain.spawn (fun () -> step1 1 it n))
  |> List.fold_left (fun acc d -> acc + Domain.join d) 0
  (* state |> List.fold_left (fun acc n -> acc + step1 1 it n) 0 *)
;;



let () =
  let input = [475449; 2599064; 213; 0; 2; 65; 5755; 51149] in
  let input = [0] in
  (* let input = [125; 17] in *)
  run input 25 |> List.length |> Printf.printf "pt01: %i\n%!" ;
  run2 input 25 |> Printf.printf "pt01: %i\n%!" ;
  run input 75 |> List.length |> Printf.printf "pt02: %i\n%!" ;
  run2 input 75 |> Printf.printf "pt02: %i\n%!" ;
