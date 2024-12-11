

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


let () =
  let input = [475449; 2599064; 213; 0; 2; 65; 5755; 51149] in
  (* let input = [125; 17] in *)
  run input 75 |> List.length |> Printf.printf "pt01: %i\n%!" ;
