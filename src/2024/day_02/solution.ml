let read_lines filename =
  let file = open_in filename in
  let rec imp acc = 
    try imp (input_line file :: acc)
    with End_of_file -> close_in file; List.rev acc
  in
  imp []
;;


type sorting = Ascending | Descending | None ;;


let sorting l =
  if l = List.sort compare l then Ascending
  else if l = List.sort (fun x y -> ~- (compare x y)) l then Descending
  else None
;;


let sorted l =
  l = List.sort compare l || l = List.sort (fun x y -> ~- (compare x y)) l
;;


let remove i l =
  let rec imp i' = function
    | [] -> []
    | _::xs when i' = i -> xs
    | x::xs -> x::(imp (i'+1) xs)
  in imp 0 l
;;


let range a b =
  let rec imp l i =
    if i < a then l else imp (i::l) (i-1)
  in imp [] (b-1)
;;


let abs_distance a b = (max a b) - (min a b) ;;


let rec count_where p = function
  | [] -> 0
  | x::xs -> (if p x then 1 else 0) + count_where p xs
;;


let rec contains_where p = function
  | [] -> false
  | x::xs -> p x || contains_where p xs
;;


let safe levels =
  if not (sorted levels) then false else
  let rec imp = function
    | _::[] | [] -> true
    | l1::l2::ls -> (
      match abs_distance l1 l2 with
        | 1 | 2 | 3 -> imp (l2::ls)
        | _ -> false
    )
  in imp levels
;;




(* let safe' tolerance level =
  let rec imp t s = function
    | _::[] | [] -> true
    | l1::l2::ls -> (
      let dist = abs_distance l1 l2 in
      let valid_dist = dist > 1 && dist <= 3 in
      let valid_step = match s with
        | Ascending -> valid_dist && l1 < l2
        | Descending -> valid_dist && l1 > l2
        | None -> ( (* we haven;t yet figured out how the list is sorted. *)
          true
        )
      let s = match s with
        | None -> (
          if l1 < l2 then Ascending else
          if l1 > l2 then Descending else
          if i1 = l2 then (if t < tolerance then imp (t+1) None (l2::ls) else false)
        )
        | Ascending | Descending as s -> s
      in
      let dist = abs_distance l1 l2 in
      match s with
      | Ascending -> if l1 < l2 then 
    )
;; *)


(* let safe'' tolerance level =
  let rec imp t = function
    | _::[] | [] as ls -> (t, ls)
    (* | l1::l2::[] ->  *)
    | l1::l2::ls -> (
      match imp t (l2::ls) with
        | (t, []) -> (t, [l1])
        | (t, (l2::ls)) -> (
          if t < 0 then (t, l1::l2::ls) else (* early return if we've exceeded the max tolerance. returned list here doesn't really matter. *)
          if abs_distance l1 l2 < 1 || abs_distance l1 l2 > 3 then ((t-1), (l2::ls)) else
          match ls with (* check sorting *)
            | [] -> (* we're at the end *)
              failwith "TODO"
            | l3::_ ->
              assert (l2 <> l3); (* we can assume that l2<>l3, since otherwise we would've remove it in the prev step *)
              let rest_sort = if l2 < l3 then Ascending else Descending in
              let is_valid = match rest_sort with
                | None -> assert false
                | Ascending -> l1 < 
        )
    )
  in
  let t = imp tolerance level in
  t > 0
;; *)


(* cringe implementation (it's essentially just brute-force :/) *)

let safe''' level =
  safe level || range 0 (List.length level) |> contains_where (fun i -> remove i level |> safe)
;;


let pt01 reports =
  reports |> count_where safe
;;


let pt02 reports =
  reports |> count_where safe'''
;;

let () =
  let reports = read_lines "input.txt" |> List.map (fun l -> l |> String.split_on_char ' ' |> List.map int_of_string) in
  reports |> pt01 |> Printf.printf "Pt01: %i\n" ;
  reports |> pt02 |> Printf.printf "Pt02: %i\n" ;
