let read_lines filename =
  let file = open_in filename in
  let rec imp acc = 
    try imp (input_line file :: acc)
    with End_of_file -> close_in file; List.rev acc
  in
  imp []
;;




type machine = (float*float) * (float*float) * (float*float) ;;



let parse input =
  let parse_button s =
    let x = String.sub s 12 2 |> float_of_string in
    let y = String.sub s 18 2 |> float_of_string in
    x,y
  in
  let parse_prize s =
    let s = String.sub s 9 (String.length s - 9) in
    match String.split_on_char ',' s with
      | x::rest ->
        let x = float_of_string x in
        let y = rest |> List.hd |> String.split_on_char '=' |> List.rev |> List.hd |> float_of_string in
        x,y
      | _ -> failwith "invalid input"
  in
  let rec imp acc = function
    | [] -> List.rev acc
    | ""::rest -> imp acc rest
    | a::b::p::rest ->
      let a = parse_button a in
      let b = parse_button b in
      let p = parse_prize p in
      imp ((a,b,p)::acc) rest
    | _ -> failwith "invalid input"
  in
  imp [] input
;;



let dump_machine (a,b,p) =
  Printf.printf "Button A: X+%f; Y+%f\n%!" (fst a) (snd a) ;
  Printf.printf "Button B: X+%f; Y+%f\n%!" (fst b) (snd b) ;
  Printf.printf "-> Prize: X=%f; Y=%f\n%!" (fst p) (snd p) ;
;;


let get arr x y=
  arr.(y).(x)
;;

let set arr x y v =
  arr.(y).(x) <- v
;;


let solve ((a,b): (int array array * int array)): (int*int) =
  Printf.printf "\n\nSOLVE\n%!" ;
  0,0
;;



let div (a,b,c) x = (a/.x, b/.x, c/.x) ;;

let add (a1,b1,c1) (a2,b2,c2) = (a1+.a2, b1+.b2, c1+.c2) ;;

let sub (a1,b1,c1) (a2,b2,c2) = (a1-.a2, b1-.b2, c1-.c2) ;;

let mul (a,b,c) x = (a*.x, b*.x, c*.x) ;;



let is_integer f =
  abs_float( f-. Float.round f) < 1e-5
;;


let solve ((ax,ay), (bx,by), (px,py)) =
  let fst (a,_,_) = a in
  let snd (_,b,_) = b in
  let trd (_,_,c) = c in
  let dump_r (x,y,z) =
    Printf.printf "%fx + %fy = %f\n%!" x y z ;
  in
  let r1 = (ax, bx, px) in
  let r2 = (ay, by, py) in
  (* Printf.printf "\n\n\n\nSOLVE\n%!" ;
  dump_r r1 ;
  dump_r r2 ; *)

  let r1 = div r1 ax in
  let r2 = sub r2 (mul r1 ay) in
  (* Printf.printf "\nstep:\n%!" ;
  dump_r r1 ;
  dump_r r2 ; *)

  let y = (trd r2) /. (snd r2) in
  let x = (trd r1) -. ((snd r1) *. y) in
  Printf.printf "--> x = %f; y = %f (res = %f; %f vs %f; %f)\n%!" x y (ax*.x+.bx*.y) (ay*.x+.by*.y) px py ;
  if is_integer x && is_integer y then (
    let x', y' = Float.to_int (Float.round x), Float.to_int (Float.round y) in
    Printf.printf "HAS SOLUTION: %i, %i\n%!" x' y' ;
    Some (x', y')
  ) else (
    Printf.printf "NO SOLUTION\n%!" ;
    None
  )
  (* 0,0 *)
;;



let pt01 (machines: machine list) =
  (* machines |> List.iter dump_machine ; *)
  machines |> List.fold_left (fun acc machine ->
    match solve machine with
      | None -> acc
      | Some (x,y) -> acc + 3*x + 1*y 
  ) 0
;;


let () =
  let machines = "input.txt" |> read_lines |> parse in
  machines |> pt01 |> Printf.printf "pt01: %i\n%!" ;
  ()
