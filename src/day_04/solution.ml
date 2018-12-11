#use "topfind" ;;
#require "extlib" ;;

let input = open_in "input.txt" |> Std.input_list



let get_chars s =
  let rec imp l = function
    | -1 -> l
    | i  -> imp (s.[i] :: l) (i-1)
  in
  imp [] (String.length s - 1)

let is_digit c = c >= '0' && c <= '9'

let parse_ints str =
  let rec imp ints curr_acc = function
    | [] -> ints @ [curr_acc]
    | x::xs ->
      if is_digit x then
        imp ints (curr_acc @ [x]) xs
      else
        imp (ints @ [curr_acc]) [] xs
  in
  imp [] [] (get_chars str)
    |> List.filter (fun l -> l <> [])
    |> List.map
      (fun digits ->
        digits
          |> List.map (fun c -> (int_of_char c) - (int_of_char '0'))
          |> List.fold_left (fun acc x -> acc * 10 + x) 0
      )



type date = {year: int; month: int; day: int; hour: int; minute: int;}

type event = BeginShift | WakeUp | FallAsleep

type record = {date: date; event: event; guard: int}

let record_to_string {date={year; month; day; hour; minute}; event; guard} =
  let event_to_string = function
    | BeginShift -> "begins shift"
    | WakeUp -> "wakes up"
    | FallAsleep -> "falls asleep"
  in
  Printf.sprintf "[%04i-%02i-%02i %02i::%02i] Guard #%04i %s" year month day hour minute guard (event_to_string event)

let parse_date str =
  let components = parse_ints str in
  match components with
  | [year; month; day; hour; minute] -> {year; month; day; hour; minute}
  | _ -> failwith "parse_date"


let date_hash {year; month; day; hour; minute} =
  (* yeah i know this is terrible, but it works ¯\_(ツ)_/¯ *)
  let imp x acc =
    acc ^ (Printf.sprintf "%02i" x)
  in
  "" |> imp year |> imp month |> imp day |> imp hour |> imp minute |> int_of_string

let date_compare a b = (date_hash a) - (date_hash b)

let process_input input =
  let mapped = input |> List.map (fun line ->
    let date_end_pos = String.index line ']' in
    let date = String.sub line 1 (date_end_pos-1) |> parse_date in
    
    let event_text = String.sub line (date_end_pos + 2) (String.length line - 2 - date_end_pos) in

    let (event, guard) = match event_text with
      | "wakes up" -> WakeUp, 0
      | "falls asleep" -> FallAsleep, 0
      | str -> BeginShift, str |> parse_ints |> List.hd
    in
    {date; event; guard}
  ) in
  let guard_id = ref 0 in
  mapped
    |> List.sort (fun {date; _} {date=date'; _} -> date_compare date date')
    |> List.map (fun ({date; event; guard} as record) ->
      match guard with
      | 0 -> {date; event; guard = !guard_id}
      | id ->
        guard_id := id ;
        record
    )




type ('key, 'value) tree = Leaf | Node of ('key * 'value) * ('key, 'value) tree * ('key, 'value) tree


let rec tree_update_f key f = function
  | Leaf -> Node ((key, f None), Leaf, Leaf)
  | Node ((key', value) as pair, lhs, rhs) ->
    if key = key' then
      Node ((key, f (Some value)), lhs, rhs)
    else if Hashtbl.hash key < Hashtbl.hash key' then
      Node (pair, tree_update_f key f lhs, rhs)
    else
      Node (pair, lhs, tree_update_f key f rhs)


let tree_to_list tree =
  let rec imp acc = function
    | Leaf -> acc
    | Node (contents, lhs, rhs) ->
      (imp [] lhs) @ [contents] @ (imp [] rhs)
  in
  imp [] tree

let range a b =
  let rec imp l i =
    if i < a then l else imp (i::l) (i-1) in
  imp [] (b-1)

let count_where f l =
  let rec imp acc = function
    | [] -> acc
    | x::xs -> if f x then imp (acc+1) xs else imp acc xs
  in
  imp 0 l

let count_of v l = l |> count_where (fun x -> x = v)

let _ =
  let records = process_input input in

  (* part 1 *)
  let start_time = ref 0 in
  let guards = records |> List.fold_left
    (fun acc {date; event; guard} ->
      match event with
      | BeginShift -> acc
      | FallAsleep -> start_time := date.minute; acc
      | WakeUp ->
        let time_asleep = date.minute - !start_time in
        tree_update_f guard (fun x -> match x with None -> time_asleep | Some x -> x + time_asleep) acc
    ) Leaf
  in

  let (id, total_sleep) = guards |> tree_to_list |> List.sort (fun (_, time) (_, time') -> time' - time) |> List.hd in

  let minutes = records
    |> List.filter (fun {date; event; guard} -> guard = id)
    |> List.fold_left (fun acc {date; event; guard} ->
        match event with
        | BeginShift -> acc
        | FallAsleep -> date.minute :: acc
        | WakeUp ->
          let (start_time, xs) = (List.hd acc, List.tl acc) in
          (range start_time date.minute) @ xs
          
      ) []
  in
  let best_minute = minutes |> List.sort (fun min min' -> (count_of min' minutes ) - (count_of min minutes)) |> List.hd in

  Printf.printf "part 1: id=%i, minute=%i, answer=%i" id best_minute (id * best_minute) ;
  