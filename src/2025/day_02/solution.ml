let read_lines filename =
  let file = open_in filename in
  let rec imp acc = 
    try imp (input_line file :: acc)
    with End_of_file -> close_in file; List.rev acc
  in
  imp []
;;

let parse_input lines =
  let input = String.concat "" lines in
  let ranges = String.split_on_char ',' input in
  ranges |> List.map (fun s ->
    let s = s |> String.split_on_char '-' |> List.map int_of_string in
    (List.nth s 0, List.nth s 1)
  )
;;

let range a b =
  Seq.unfold (fun a -> if a >= b then None else Some (a,a+1)) a
;;


let invalid n s =
  let len = String.length s in
  if len mod n <> 0 then
    false
  else
    let sub_len = len / n in
    let indices = range 0 sub_len in
    Seq.for_all (fun idx ->
      let c = s.[idx] in
      Seq.for_all (fun i -> c = s.[idx + i * sub_len]) (range 1 (n))
    ) indices
;;


let run invalid ranges =
  let rec imp acc = function
    | [] -> acc
    | (s,e)::ranges ->
      let acc = range s (e+1) |> Seq.fold_left (fun acc n ->
        if invalid (string_of_int n) then
          acc + n
        else
          acc
      ) acc in
      imp acc ranges
  in
  imp 0 ranges
;;


let () = 
  let ranges = read_lines "input.txt" |> parse_input in
  ranges |> run (invalid 2) |> Printf.printf "Pt01: %i\n" ;
  ranges |> run (fun s ->
    range 2 (String.length s + 1) |> Seq.exists (fun n -> invalid n s)
  ) |> Printf.printf "Pt02: %i\n" ;
