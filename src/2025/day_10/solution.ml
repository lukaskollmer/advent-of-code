(* opam exec -- ocamlfind ocamlopt -package Z3 -linkpkg -O3 solution.ml *)

let read_lines filename =
  let file = open_in filename in
  let rec imp acc = 
    try imp (input_line file :: acc)
    with End_of_file -> close_in file; List.rev acc
  in
  imp []
;;


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


let pt02 (input: (indicator_light list * int list list * int list) list) =
  let run machine =
    let (_, buttons, joltages) = machine in
    let ctx = Z3.mk_context [("model", "true")] in
    let vars = buttons |> List.mapi (fun i _ ->
      Z3.Arithmetic.Integer.mk_const_s ctx ("v" ^ string_of_int i)
    ) in
    let opt = Z3.Optimize.mk_opt ctx in
    let exprs = joltages |> List.mapi (fun iJ j ->
      let add = buttons
        |> List.mapi (fun i x -> (i,x))
        |> List.fold_left (fun acc (iB,b) ->
          if List.mem iJ b then
            (List.nth vars iB)::acc
          else
            acc
        ) [] in
        let add = Z3.Arithmetic.mk_add ctx add in
        Z3.Boolean.mk_eq ctx add (Z3.Arithmetic.Integer.mk_numeral_i ctx j)
    ) in
    Z3.Optimize.add opt exprs ;
    vars |> List.map (fun v ->
      Z3.Arithmetic.mk_ge ctx v (Z3.Arithmetic.Integer.mk_numeral_i ctx 0)
    ) |> Z3.Optimize.add opt ;
    let _ = Z3.Optimize.minimize opt (Z3.Arithmetic.mk_add ctx vars) in
    let status = Z3.Optimize.check opt in
    assert (status = Z3.Solver.SATISFIABLE) ;
    let model = Z3.Optimize.get_model opt |> Option.get in
    vars
      |> List.map (fun v -> Z3.Model.eval model v true |> Option.get |> Z3.Arithmetic.Integer.numeral_to_string |> int_of_string)
      |> List.fold_left (+) 0
  in
  input
    |> chunked'
    |> List.map (fun machines ->
      Domain.spawn (fun () -> machines |> List.fold_left (fun acc m -> acc + (run m)) 0)
    )
    |> List.fold_left (fun acc d -> acc + Domain.join d) 0
;;

let () =
  let input = read_lines "input.txt" |> parse_input in
  input |> pt01 |> Printf.printf "Pt01: %i\n%!" ;
  input |> pt02 |> Printf.printf "Pt02: %i\n%!" ;
