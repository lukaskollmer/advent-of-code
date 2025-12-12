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



let parse_input lines =
  let parse_line s =
    let s = String.split_on_char ':' s in
    let src = List.hd s in
    let dsts = s |> List.tl |> List.hd |> String.trim |> String.split_on_char ' ' in
    (src, dsts)
  in
  lines |> List.map parse_line
;;

type path = string list ;;

module PathSet = Set.Make(struct
  type t = path
  (* let rec compare p1 p2 =
    match p1, p2 with
      | [], [] -> true
      |  *)
  let compare = compare
end) ;;


module H = Hashtbl.Make(String) ;;

let pt01 input =
  let cons = H.create 32 in
  input |> List.iter (fun (src, dsts) -> H.add cons src dsts) ;
  let rec imp (cur_path: path) (paths: PathSet.t) = function
    | "out" -> PathSet.add (List.rev ("out"::cur_path)) paths
    | cur ->
      let next = H.find cons cur in
      next |> List.fold_left (fun paths next ->
        imp (cur::cur_path) paths next
      ) paths
  in
  let paths = imp [] PathSet.empty "fft" in
  paths |> PathSet.cardinal
;;



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


let pt02 input =
  let cons = H.create 32 in
  input |> List.iter (fun (src, dsts) -> H.add cons src dsts) ;
  let rec imp (s1,s2,cur,acc) =
    let s1 = s1 || cur = "dac" in
    let s2 = s2 || cur = "fft" in
    if s1 && s2 then 
      acc + 1
    else if cur = "out" then 0 else
    (* assert (not (List.mem cur cur_path)) ; *)
    let next = H.find cons cur in
    next |> List.fold_left (fun acc next ->
      acc + imp (s1, s2, next,acc)
    ) acc
  in
  (* let imp = memo_rec imp in *)
  if false then (
    H.find cons "svr" |> List.map (fun n ->
    Domain.spawn (fun () -> imp (false, false, n,0))
  ) |> List.fold_left (fun acc n -> acc + Domain.join n) 0
  ) else (
    imp (false, false, "svr",0)
  )
;;



let () =
  Printexc.record_backtrace true ;
  let input = read_lines "input.txt" |> parse_input in
  input |> pt01 |> Printf.printf "Pt01: %i\n%!" ;
  (* input |> pt02 |> Printf.printf "Pt02: %i\n%!" ; *)
