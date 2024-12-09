let read_lines filename =
  let file = open_in filename in
  let rec imp acc = 
    try imp (input_line file :: acc)
    with End_of_file -> close_in file; List.rev acc
  in
  imp []
;;



let repeat n x =
  List.init n (fun _ -> x)
;;

let third (_,_,c) = c ;;


type block =
  | Free
  | File of int (* file id*)
;;


let dump_block block =
  block |> Array.iter (function
    | Free -> print_char '.'
    | File id -> Printf.printf "%i" id
  ) ;
  print_newline ();
;;

(* Part 01 *)

let parse input =
  input |> String.fold_left (fun (idx,file_id,acc) c ->
    let size = c |> Seq.return |> String.of_seq |> int_of_string in
    let is_file = (mod) idx 2 = 0 in
    if is_file then
      (idx+1, file_id+1, acc @ (repeat size (File file_id)))
    else
      (idx+1, file_id, acc @ (repeat size Free))
  ) (0,0,[]) |> third |> Array.of_list
;;


let checksum blocks =
  Array.fold_left (fun (idx,sum) -> function
    | Free -> (idx + 1,sum)
    | File id -> (idx + 1,sum + idx * id)
  ) (0,0) blocks |> snd
;;



let pt01 input =
  let blocks = parse input in
  let rec imp src_idx =
    if src_idx < 0 then () else
    if blocks.(src_idx) = Free then imp (src_idx - 1) else
    match Array.find_index ((=) Free) blocks with
      | None -> () (* it's over *)
      | Some dst_idx when dst_idx < src_idx -> (
        (* Printf.printf "%03i <- %03i\n%!" dst_idx src_idx ; *)
        blocks.(dst_idx) <- blocks.(src_idx) ;
        blocks.(src_idx) <- Free ;
        imp (src_idx - 1)
      )
      | Some _ -> ()
  in
  (* Printf.printf "Will run!\n%!" ; *)
  imp (Array.length blocks - 1);
  blocks |> checksum
;;



(* Part 02 *)


let parse' input =
  input |> String.fold_left (fun (idx,file_id,acc) c ->
    let size = c |> Seq.return |> String.of_seq |> int_of_string in
    if size = 0 then (idx+1,file_id,acc) else
    let is_file = (mod) idx 2 = 0 in
    if is_file then
      (idx+1, file_id+1, (size, File file_id) :: acc)
    else
      (idx+1, file_id, (size, Free) :: acc)
  ) (0,0,[]) |> third |> List.rev |> Array.of_list
;;

let explode blocks =
  blocks |> Array.fold_left (fun acc (n, x) -> acc @ (repeat n x)) []
;;

let arr_insert arr is =
  let adj_idx = ref 0 in
  Array.init (Array.length arr + List.length is) (fun idx ->
    match List.find_opt (fun (insert_idx, elem) -> insert_idx = idx) is with
      | None -> let v = arr.(!adj_idx) in incr adj_idx ; v
      | Some (_, elem) -> (elem)
  )
;;

let swap (arr: (int*block) array ref) dst src =
  let (dst_len, dst_val) = !arr.(dst) in
  let (src_len, src_val) = !arr.(src) in
  if dst_len = src_len then (
    !arr.(dst) <- (dst_len, src_val) ;
    !arr.(src) <- (src_len, dst_val) ;
  ) else if dst_len > src_len then (
    (* we need to split the dst block into 2, and the src block into 2 *)
    !arr.(dst) <- (src_len, src_val) ;
    !arr.(src) <- (src_len, dst_val) ;
    Printf.printf "arr_insert\n%!" ;
    arr := arr_insert !arr [(dst+1, (dst_len - src_len, Free))] ;
    (* arr := arr_insert !arr [(dst+1, (dst_len - src_len, Free))] *)
  ) else (
    assert (dst_len < src_len) ;
    assert false ;
  )
;;

let array_swap arr dst src len =
  for o=0 to len-1 do
    assert (dst+len < src) ;
    let v = arr.(dst+o) in
    arr.(dst+o) <- arr.(src+o) ;
    arr.(src+o) <- v ;
  done
;;

let rec all_in_range arr start_pos end_pos f =
  if start_pos = end_pos then f arr.(start_pos) else
  f arr.(start_pos) && all_in_range arr (start_pos+1) end_pos f
;;


let pt02 input =
  let blocks = ref (parse' input) in
  let rec imp id =
    Printf.printf "id: %i\n%!" id ;
    (* !blocks |> explode |> Array.of_list |> dump_block ; *)
    match Array.find_index (function _, Free -> false | _, File id' -> id = id') !blocks with
      | None -> (Printf.printf "Unable to find id %i\n%!" id)
      | Some src_idx ->
    if src_idx < 0 then (Printf.printf "EARLU RETURN1\n%!") else
    match !blocks.(src_idx) with
      | (_, Free) -> imp (id - 1)
      | (len, File id) -> (
        let dst_idx = Array.find_index (function
          | (_, File _) -> false
          | (size, Free) -> size >= len
        ) !blocks in
        match dst_idx with
          | None -> imp (id - 1)
          | Some dst_idx when dst_idx < src_idx -> (
            swap blocks dst_idx src_idx ;
            imp (id - 1)
          )
          | Some _ -> imp (id-1)
      )
  in
  let max_id = Array.fold_left (fun acc -> function _, Free -> acc | _, File id -> max acc id) 0 !blocks in
  imp max_id ;
  !blocks |> explode |> Array.of_list |> checksum
;;




let () =
  let input = "input.txt" |> read_lines |> List.hd in
  (* let blocks = "12345" |> parse in *)
  input |> pt01 |> Printf.printf "pt01: %i\n%!" ;
  input |> pt02 |> Printf.printf "pt02: %i\n%!" ;
