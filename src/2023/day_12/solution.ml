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

let repeat' n sep l =
  match n with
    | n when n <= 0 -> []
    | 1 -> l
    | n -> begin
      let rec imp acc = function
        | 0 -> acc
        (* | 1 -> acc @ l *)
        | n -> imp (acc @ [sep] @ l) (n-1)
      in imp l (n-1)
    end
;;

let rec join sep = function
  | [] -> []
  | x::xs -> x @ sep :: join sep xs
;;



let time msg f =
  let t0 = Sys.time () in
  let r = f () in
  let t1 = Sys.time () in
  Printf.printf "%s: %f\n%!" msg (t1 -. t0) ;
  r
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



let springs_to_string = function
  | [] -> "[]"
  | springs -> springs |> List.map (function Operational -> "." | Broken -> "#" | Unknown -> "?") |> String.concat ""
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
  in
  let r = imp (springs, counts) in
  if r <> satisfies_counts' counts springs then
    Printf.printf "AAAAAAAAAAAAA %s %s\n%!" (springs_to_string springs) (counts |> List.map string_of_int |> String.concat ",") ;
  r
;;



let dump_entry_list l =
  l |> List.map (function Operational -> '.' | Broken -> '#' | Unknown -> '?') |> List.iter (Printf.printf "%c") ;
  Printf.printf "\n" ;
;;

let all_perms l =
  let rec imp = function
    | [] -> []
    | [Unknown] -> [[Operational]; [Broken]]
    | [x] -> [[x]]
    | Unknown::xs ->
      let a = xs |> imp |> List.map (List.cons Broken) in
      let b = xs |> imp |> List.map (List.cons Operational) in
      a @ b
    | x::xs ->
      xs |> imp |> List.map (List.cons x)
  in
  imp l
;;


let fold_perms f acc l =
  let rec imp acc springs = function
    | [] -> acc
    | [Unknown] -> 
      let acc = f acc (List.rev (Operational::springs)) in
      let acc = f acc (List.rev (Broken::springs)) in
      acc
    | [x] -> f acc (List.rev (x::springs))
    | Unknown::xs ->
      let acc = imp acc (Broken::springs) xs in
      let acc = imp acc (Operational::springs) xs in
      acc
    | x::xs -> imp acc (x::springs) xs
  in
  imp acc [] l
;;



let calc_row' springs counts =
  fold_perms (fun acc springs ->
    if satisfies_counts' counts springs
    then begin
      dump_entry_list springs ;
      acc+1 end
    else acc
  ) 0 springs
;;




module RowCalcHash = struct
  type t = (entry list * int list)
  let equal (a: t) (b: t): bool = a = b
  (* let hash (i: int) = i land max_int *)
  let hash (springs, counts) =
    let hash = List.fold_left (fun acc x -> acc lxor (match x with Operational -> 1 | Broken -> 2 | Unknown -> 3)) max_int springs in
    hash lxor List.fold_left (fun acc x -> acc lxor x) max_int counts
end

module RowCalcHashTbl = Hashtbl.Make(RowCalcHash)



let memo_rec f =
  let h = Printf.printf "MAKE HASHTBL\n%!" ; RowCalcHashTbl.create 16 in
  let rec g x =
    try RowCalcHashTbl.find h x
    with Not_found ->
      let y = f g x in
      RowCalcHashTbl.add h x y;
      y
  in
  g
;;


let drop_first n l =
  let rec imp = function
    | (0, xs) -> xs
    | (_, []) -> []
    | (n, x::xs) -> imp (n-1, xs)
  in
  imp (n, l)
;;

let take n l =
  let rec imp acc = function
    | (0, _) -> List.rev acc
    | (_, []) -> List.rev acc
    | (n, x::xs) -> imp (x::acc) (n-1, xs)
in imp [] (n, l)
;;

let first_n_satisfy n f l =
  let rec imp = function
    | (0, _) -> true
    | (_, []) -> false
    | (n, x::xs) -> f x && imp (n-1, xs)
  in imp (n, l)
;;

let subrange start len l =
  let rec imp acc = function
    | (0, _) -> List.rev acc
    | (_, []) -> List.rev acc
    | (n, x::xs) -> imp (x::acc) (n-1, xs)
  in imp [] (len, drop_first start l)
;;




let calc_row'' self = function
    | [], [] -> 1
    | [], _ -> 0
    | Operational::springs, counts -> self (springs, counts)
    (* | Broken::_, [] -> 0 *)
    | Broken::_, [] -> 0
    (* | Broken::Operational::springs, 1::counts -> self (Operational::springs, counts) *)
    | ((Broken::_) as springs), c::counts ->
      (* begin match counts with
        | [] -> 0
        (* | 1::[] -> self springs [] *)
        | 1::counts -> (
          match springs with
            | Operational::springs -> self springs counts
            | Broken::_ -> 0
            | Unknown::springs ->
              (* we assume that the next spring is operational, and just skip it *)
              self springs counts
            | [] -> if counts = [] then 1 else 0
        )
        | n::counts -> self springs (n-1::counts)
      end *)
      begin
        (* let springs = Broken::springs in *)
        (* let x1 = List.length springs >= c in
        let x2 = first_n_satisfy c ((<>) Operational) springs in
        (* let x2 = springs |>  *)
        (* let x3 = match List.nth_opt springs c with None -> true | Some x -> x <> Broken in *)
        let x3 = (List.length springs = c) || (List.nth springs c <> Broken) in *)
        if (List.length springs >= c) && (first_n_satisfy c ((<>) Operational) springs) && ((List.length springs = c) || (List.nth springs c <> Broken)) then
          self (drop_first (c+1) springs, counts)
        else
          (* begin Printf.printf "DID NOT EVAL TO TRUE FFFFF (%b, %b, %b)\n%!" x1 x2 x3; *)
          0
        (* end *)
      end
    | Unknown::springs, counts ->
      (self (springs, counts)) + (self (Broken::springs, counts))
;;


let calc_rowwwwww self (springs, count) =
  let r = calc_row'' self (springs, count) in
  (* begin
    let r' = calc_row' springs count in
    Printf.printf "calc_row'' %s %s = %d (%b; old: %d, new: %d)\n%!"
      (springs_to_string springs)
      (count |> List.map string_of_int |> String.concat ",")
      r
      (r = (r'))
      (r')
      r
    ;
  end ; *)
  r
  (* calc_row' springs count *)
;;

(* let calc_row'' = memo_rec calc_row'' ;; *)
(* let calc_row'' = memo_rec calc_rowwwwww ;; *)


let take n l =
  let rec imp acc = function
    | (0, _) -> List.rev acc
    | (_, []) -> List.rev acc
    | (n, x::xs) -> imp (x::acc) (n-1, xs)
  in imp [] (n, l)
;;

let chunked n l =
  if n = 0 then [l] else
  let rec imp acc = function
    | [] -> List.rev acc
    | l -> imp ((take n l)::acc) (drop_first n l)
in imp [] l
;;


let run (input: (entry list * int list) list) =
  (* input
    |> List.mapi (fun idx (springs, counts) ->
      Printf.printf "IDX %d\n%!" idx ;
      let calc_row'' = memo_rec calc_rowwwwww in
      (* Domain.spawn (fun () -> calc_row'' springs counts) *)
      Domain.spawn (fun () -> let r = calc_row'' springs counts in Printf.printf "IDX %d DONE\n%!" idx ; r )
    )
    |> List.map Domain.join
    |> List.fold_left (+) 0 *)
  Printf.printf "RUN\n%!" ;
  (* let progress_mutex = Mutex.create () in
  let progress = ref 0 in
  let incr_progress () =
    Mutex.lock progress_mutex ;
    incr progress ;
    let p = !progress in
    Mutex.unlock progress_mutex ; p
  in *)
  let x = input
    |> List.mapi (fun idx (a,b) -> (idx, a, b))
    (* |> subrange 0 500 *)
  in
  Printf.printf "will make chunks\n%!" ;
  let x = x |> chunked (List.length x / 10) in
  Printf.printf "did make chunks\n%!" ;
  let x = x
    |> inspect (fun chunks -> Printf.printf "CHUNKS: %d\n%!" (List.length chunks))
    |> List.mapi (fun idx chunk -> (idx, chunk)) in
  x |> List.fold_left (fun acc (chunk_idx, chunk) ->
      let calc_row'' = memo_rec calc_rowwwwww in
      (* Domain.spawn (fun () -> chunk |> List.map (fun (springs, counts) -> calc_row'' springs counts) |> List.fold_left (+) 0)::acc *)
      (* Domain.spawn (fun () -> 0)::acc *)
      Printf.printf "Running Domain for chunk idx %d\n%!" chunk_idx;
      let d: int Domain.t = Domain.spawn (fun () ->
        let x: int = chunk |> List.mapi (fun i (_idx, springs, counts) ->
          (* let r = if _idx >= 500 then calc_row'' springs counts else 0 in *)
          let r = calc_row'' (springs, counts) in
          Printf.printf "ROW %s %s = %d\n%!" (springs_to_string springs) (counts |> List.map string_of_int |> String.concat ",") r ;
          (* Printf.printf "Chunk %d.%d is DONE (overall progress: %d)\n%!" chunk_idx i (incr_progress ()) ; r *)
          (* Printf.printf "Chunk %d.%d is DONE (overall progress: %d)\n%!" chunk_idx i (incr_progress ()) ; r *)
          Printf.printf "Chunk %d.%d is DONE\n%!" chunk_idx i ; r
        ) |> List.fold_left (+) 0 in
        Printf.printf "chunk idx %d IS DONE\n%!" chunk_idx;
        x
      ) in
      d::acc
    ) []
    |> List.map Domain.join
    |> List.fold_left (+) 0
;;


let pt01 input =
  let n = List.length input in
  (* input
    |> parse_input
    |> List.mapi (fun idx (springs, counts) ->
      let p = time (Printf.sprintf "\n%3d/%3d" idx n) (fun () -> calc_row'' springs counts) in
      Printf.printf "- %s %s = %d\n%!" (springs_to_string springs) (counts |> List.map string_of_int |> String.concat ",") p ;
      p
    )
    |> List.fold_left (+) 0 *)
    run (input |> parse_input)
;;


let pt02 input =
  let n = List.length input in
  (* let i = ref 0 in
  input
    |> parse_input
    |> List.map (fun (springs, counts) -> (repeat' 5 Unknown springs, repeat 5 counts))
    |> List.fold_left (fun acc (springs, counts) ->
      (* acc + calc_row' springs counts *)
      let x = time (Printf.sprintf "%3d/%3d" !i n) (fun () -> calc_row'' springs counts) in
      incr i ; acc + x
    ) 0 *)
  input
    |> parse_input
    |> List.map (fun (springs, counts) -> (repeat' 5 Unknown springs, repeat 5 counts))
    |> run
;;



let () =
  let input = read_lines "input.txt" in
  (* input |> pt01 |> Printf.printf "Pt01: %d\n%!" ; *)
  input |> pt02 |> Printf.printf "Pt02: %d\n%!" ;
