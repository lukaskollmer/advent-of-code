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
    | i  -> imp (s.[i] :: l) (i-1)
  in
  imp [] (String.length s - 1)
;;


let flat_map f l =
  let rec imp acc = function
    | [] -> acc
    | x::xs -> imp (acc @ f x) xs
in imp [] l
;;


let inspect f l = List.iter f l ; l ;;

let count_where f l =
  let rec imp acc = function
    | [] -> acc
    | x::xs -> imp (if f x then acc+1 else acc) xs
  in imp 0 l
;;

let repeat n l =
  let rec imp acc = function
    | 0 -> acc
    | n -> imp (acc @ l) (n-1)
  in imp [] n
;;

let rec join sep = function
  | [] -> []
  | x::xs -> x @ sep :: join sep xs
;;




type entry = Operational | Broken | Unknown ;;


let parse_input input =
  let parse_row row =
    let row = row |> String.split_on_char ' ' in
    let x = row |> List.hd |> get_chars |> List.map (function '.' -> Operational | '#' -> Broken | '?' -> Unknown) in
    let y = List.nth row 1 |> String.split_on_char ',' |> List.map int_of_string in
    (x, y)
  in
  input |> List.map parse_row
;;



let springs_to_string springs =
  springs |> List.map (function Operational -> "." | Broken -> "#" | Unknown -> "?") |> String.concat ""
;;



let satisfies_counts counts springs =
  let rec imp = function
    | ([], []) -> true
    | ([], _) -> false
    | (Operational::springs, counts) -> imp (springs, counts)
    | ([Broken], [1]) -> true
    | (Broken::springs, count::counts) ->
          count > 0
        && List.length springs >= count
        && (match count with
          | 1 -> (if List.length springs > 0 then List.hd springs = Operational else true)
          (* | _ -> (List.length springs >= (count+1) && List.hd springs = Broken) *)
          | _ -> true
          )
        (* && (if count = 1 then List.hd springs = Operational else true) *)
        && (if count > 1 then List.hd springs = Broken else true)
        && imp (springs, if count = 1 then counts else ((count-1)::counts))
    | (Unknown::_, _) -> false
    | (Broken::_, _) -> false
  in imp (springs, counts)
;;


let satisfies_counts' counts springs =
  assert (not (List.mem Unknown springs)) ;
  let broken_segments = springs
    |> springs_to_string
    |> String.split_on_char '.'
    |> List.map String.length
    |> List.filter ((<>) 0)
  in
  (* Printf.printf "[satisfies_counts']\n" ;
  Printf.printf "- broken_segments: %s\n" (broken_segments |> List.map string_of_int |> String.concat ",") ;
  Printf.printf "- counts:          %s\n" (counts |> List.map string_of_int |> String.concat ",") ; *)
  broken_segments = counts
;;


let dump_entry_list l =
  l |> List.map (function Operational -> '.' | Broken -> '#' | Unknown -> '?') |> List.iter (Printf.printf "%c") ;
  Printf.printf "\n" ;
;;

let all_perms l =
  let rec imp i = function
    | [] -> []
    | [Unknown] -> [[Operational]; [Broken]]
    | [x] -> [[x]]
    | Unknown::xs ->
      (* Printf.printf "Unknown (at %d)\n" i ; *)
      let a = xs |> imp (i+1) |> List.map (List.cons Broken) in
      let b = xs |> imp (i+1) |> List.map (List.cons Operational) in
      (* Printf.printf "a[at %d]: " i ; a |> List.flatten |> dump_entry_list ;
      Printf.printf "b[at %d]: " i ; b |> List.flatten |> dump_entry_list ; *)
      (* [a |> List.flatten; b |> List.flatten] *)
      a @ b
    | x::xs ->
      (* Printf.printf "Known (at %d)\n" i ; *)
      let r = xs |> imp (i+1) |> List.map (List.cons x) in
      (* Printf.printf "#r[at %d]: %d\n" i (List.length r) ;
      Printf.printf "r[at %d].lens: " i ; r |> List.map (List.length) |> List.iter (Printf.printf "%d ") ; Printf.printf "\n" ;
      Printf.printf "r[at %d]: " i ; r |> List.flatten |> dump_entry_list ; *)
      r
  in
  imp 0 l
;;



let calc_row (springs, counts) =
  (* Printf.printf "[calc_row] springs: " ; dump_entry_list springs ; *)
  let perms = all_perms springs in
  (* Printf.printf "#springs: %d\n" (List.length perms) ; *)
  (* perms |> List.map (fun p -> 
    Printf.printf "\n" ;
    dump_entry_list p ;
    (* Printf.printf "Satisfies counts: %b\n" (satisfies_counts counts p) ; *)
  ); *)
  (* failwith "TODO" ;
  (* let rec imp acc = function
    | *) *)

  (* let n = perms
    |> List.filter (satisfies_counts counts)
    |> inspect (fun p -> Printf.printf "Perm: %s\n" (springs_to_string p))
    |> List.length
  in
  Printf.printf "[calc_row] springs: %s = %d\n" (springs_to_string springs) n ;
  n *)

  (* if List.length springs = 15 then begin
    Printf.printf "All Perms for %s:\n" (springs_to_string springs);
    perms |> List.iter (fun perm ->
      Printf.printf "Perm: %s\n = %b\n" (springs_to_string perm) (satisfies_counts' counts perm) ;
    )
  end ; *)

  let perms = perms |> List.filter (satisfies_counts' counts) in
  let n = List.length perms in
  (* Printf.printf "[calc_row] springs: %s %s = %d\n%!" (springs_to_string springs) (counts |> List.map string_of_int |> String.concat ",") n ; *)
  (* inspect (fun p -> Printf.printf "- %s (#=%d)\n" (springs_to_string p) (count_where ((=) p) perms)) perms ; *)
  n
;;





let pt01 input =
  input
    |> parse_input
    (* |> List.map calc_row *)
    |> List.mapi (fun idx r -> Printf.printf "%3d / %3d\n%!" idx (List.length input) ; calc_row r)
    |> List.fold_left (+) 0
;;


let pt02 input =
  failwith "TODO" ;
  (* let i = parse_input input in
  let r = List.map (fun (springs, counts) ->
    let x = repeat 5 springs in
    let y = repeat 5 counts in (true, true)
    (* (repeat 5 springs |> join Unknown, repeat 5 counts |> List.flatten) *)
  ) i in *)

  let rows = input |> parse_input
    |> List.map (fun (springs, counts) ->
      let x = repeat 5 springs in
      let y = repeat 5 counts in (x, y)
      (* (repeat 5 springs |> join Unknown, repeat 5 counts |> List.flatten) *)
    )
  in

  let max_unknowns = rows |> List.fold_left (fun acc (springs, _) ->
    springs |> count_where ((=) Unknown) |> max acc
  ) 0 in
  Printf.printf "max_unknowns: %d\n%!" max_unknowns ;

  failwith "TODO" ;

  let n = List.length input in 0

  (* input |> List.fold_left (fun acc (springs, counts) ->
    let springs = repeat 5 springs in
    let counts = repeat 5 counts in
    (* if satisfies_counts' counts springs then acc+1 else acc *)
    acc + calc_row (springs, counts)
  ) 0 *)

  (* (* input
    |> parse_input *)
    (* |> List.map calc_row *)
    rows
    |> List.mapi (fun idx r -> Printf.printf "%3d / %3d\n%!" idx (List.length rows) ; calc_row r)
    |> List.fold_left (+) 0 *)
;;


let () =
  let input = read_lines "input.txt" in
  input |> pt01 |> Printf.printf "Pt01: %d\n%!" ;
  input |> pt02 |> Printf.printf "Pt02: %d\n%!" ;
