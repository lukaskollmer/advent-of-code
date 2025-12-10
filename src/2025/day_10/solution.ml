(* ocamlopt -g -O3 -I +unix unix.cmxa solution.ml *)

let read_lines filename =
  let file = open_in filename in
  let rec imp acc = 
    try imp (input_line file :: acc)
    with End_of_file -> close_in file; List.rev acc
  in
  imp []
;;

(* let measure l f =
  let start = Unix.gettimeofday () in
  let result = f () in
  let stop = Unix.gettimeofday () in
  Printf.printf "[MEASURE] %s took %f sec\n%!" l (stop -. start) ;
  result
;; *)


type indicator_light = Off | On ;;

let parse_input lines =
  let parse_line l =
    let l = String.split_on_char ' ' l in
    let (hd,tl) = match l with (hd::tl) -> (hd,tl) | _ -> failwith "" in
    let lights = hd
      |> String.fold_left (fun acc c ->
        match c with '.' -> Off::acc | '#' -> On::acc | _ -> acc
      ) []
      |> List.rev
    in
    let parse_int_list s =
      String.sub s 1 (String.length s - 2)
        |> String.split_on_char ',' |> List.map int_of_string
    in
    let buttons = tl
      |> List.take (List.length tl - 1)
      |> List.map parse_int_list
    in
    let joltages = List.nth tl (List.length tl - 1) |> parse_int_list in
    (lights,buttons,joltages)
  in
  lines |> List.map parse_line
;;


let join s l =
  let rec imp acc = function
    | [] -> acc
    | x::xs -> imp (acc ^ s ^ x) xs
  in
  match l with
    | [] -> ""
    | [x] -> x
    | x::xs -> imp x xs
;;


let chunked n l =
  if n = 0 then [l] else
  let rec imp acc = function
    | [] -> List.rev acc
    | l -> imp ((List.take n l)::acc) (List.drop n l)
  in imp [] l
;;

let chunked' l = chunked (Domain.recommended_domain_count ()) l;;


let range a b =
  Seq.unfold (fun a -> if a >= b then None else Some (a,a+1)) a
;;


let toggle l i =
  l.(i) <- match l.(i) with On -> Off | Off -> On
;;


let pt01 (input: (indicator_light list * int list list * int list) list) =
  let run machine =
    let (lights, buttons, _) = machine in
    let lights = Array.of_list lights in
    let num_lights = Array.length lights in
    let num_buttons = List.length buttons in
    let max = (Int.shift_left 1 num_buttons) - 1 in
    range 0 (max+1) |> Seq.fold_left (fun acc mask ->
      let state = Array.make num_lights Off in
      let num_on = range 0 num_buttons |> Seq.fold_left (fun acc i ->
        let ison = Int.logand mask (Int.shift_left 1 i) <> 0 in
        if ison then (
          let button = List.nth buttons i in
          button |> List.iter (fun i -> toggle state i) ;
          acc + 1
        ) else acc
      ) 0 in
      if Array.equal (=) state lights then (
        Int.min acc num_on
      ) else
        acc
    ) Int.max_int
  in
  input
    |> chunked'
    |> List.map (fun machines ->
      Domain.spawn (fun () -> machines |> List.fold_left (fun acc m -> acc + (run m)) 0)
    )
    |> List.fold_left (fun acc d -> acc + Domain.join d) 0
;;


let pt02 input = 12 ;;

let () =
  Printexc.record_backtrace true ;
  let input = read_lines "input.txt" |> parse_input in
  input |> pt01 |> Printf.printf "Pt01: %i\n%!" ;
  (* input |> pt02 |> Printf.printf "Pt02: %i\n%!" ; *)
