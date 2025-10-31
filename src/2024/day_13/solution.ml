let rec gcd u v =
  if v <> 0 then (gcd v (u mod v))
  else (abs u)
;;

let lcm m n =
  match m, n with
  | 0, _ | _, 0 -> 0
  | m, n -> abs (m * n) / (gcd m n)
;;


module Rat = struct
  type t = int * int

  let zero = 0,1
  let one = 1,1

  let normalise (x,y) =
    if y = 0 then  failwith "/0 not allowed!" ;
    let (num, denom) = if y < 0 then (-x, -y) else (x, y) in
    let divisor = gcd num denom in
    (num / divisor, denom / divisor)
  
  let add (x,y) (x',y') =
    let lcm = lcm y y' in
    let x = (x*(lcm/y) + x'*(lcm/y')) in
    (x, lcm) |> normalise
  
  let mul (x,y) (x',y') =
    (x*x', y*y') |> normalise
  
  let neg (x,y) = (-x,y)

  
  let sub a b = add a (neg b)

  let inv (x,y) = (y,x)

  let div a b = mul a (inv b)

  let of_int x = x,1

  let to_int (x,y) = assert (y = 1) ; x

  let to_int_opt (x,y) =
    if y = 1 then Some x else None

  let is_int (_,y) = y = 1

  let to_float (x,y) = (float_of_int x) /. (float_of_int y)

  let to_string (x,y) =
    if y = 0 then
      string_of_int x
    else
      string_of_float (to_float (x,y))
end ;;



let read_lines filename =
  let file = open_in filename in
  let rec imp acc = 
    try imp (input_line file :: acc)
    with End_of_file -> close_in file; List.rev acc
  in
  imp []
;;


type machine = (Rat.t*Rat.t) * (Rat.t*Rat.t) * (Rat.t*Rat.t) ;;



let parse input =
  let parse_button s =
    let x = String.sub s 12 2 |> int_of_string |> Rat.of_int in
    let y = String.sub s 18 2 |> int_of_string |> Rat.of_int in
    x,y
  in
  let parse_prize s =
    let s = String.sub s 9 (String.length s - 9) in
    match String.split_on_char ',' s with
      | x::rest ->
        let x = x |> int_of_string |> Rat.of_int in
        let y = rest |> List.hd |> String.split_on_char '=' |> List.rev |> List.hd |> int_of_string |> Rat.of_int in
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



let div (a,b,c) x =
  (Rat.div a x, Rat.div b x, Rat.div c x)
;;

let add (a1,b1,c1) (a2,b2,c2) =
  (Rat.add a1 a2, Rat.add b1 b2, Rat.add c1 c2)
;;

let sub (a1,b1,c1) (a2,b2,c2) =
  (Rat.sub a1 a2, Rat.sub b1 b2, Rat.sub c1 c2)
;;

let mul (a,b,c) x =
  (Rat.mul a x, Rat.mul b x, Rat.mul c x)
;;



let solve ((ax,ay), (bx,by), (px,py)) =
  let snd (_,b,_) = b in
  let trd (_,_,c) = c in

  let r1 = (ax, bx, px) in
  let r2 = (ay, by, py) in

  let r1 = div r1 ax in
  let r2 = sub r2 (mul r1 ay) in

  let y = Rat.div (trd r2) (snd r2) in
  let x = Rat.sub (trd r1) (Rat.mul (snd r1) y) in
  match Rat.to_int_opt x, Rat.to_int_opt y with
    | Some x, Some y -> Some (x,y)
    | _ -> None
;;



let adj_machine (a,b,(p1,p2)) adj =
  let adj = Rat.of_int adj in
  (a,b,(Rat.add p1 adj, Rat.add p2 adj)) ;;


let run adj (machines: machine list) =
  machines |> List.fold_left (fun acc machine ->
    match solve (adj_machine machine adj) with
      | None -> acc
      | Some (x,y) -> acc + 3*x + y 
  ) 0
;;


let () =
  let (machines: machine list) = "input.txt" |> read_lines |> parse in
  machines |> run 0 |> Printf.printf "pt01: %i\n%!" ;
  machines |> run 10000000000000 |> Printf.printf "pt02: %i\n%!" ;
  ()
