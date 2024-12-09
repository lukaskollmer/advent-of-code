let read_lines filename =
  let file = open_in filename in
  let rec imp acc = 
    try imp (input_line file :: acc)
    with End_of_file -> close_in file; List.rev acc
  in
  imp []
;;


type block =
  | Free
  | File of int (* file id*)
;;


let repeat n x =
  List.init n (fun _ -> x)
;;

let third (_,_,c) = c ;;


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


let pt01 blocks =
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


let () =
  let blocks = "input.txt" |> read_lines |> List.hd |> parse in
  (* let blocks = "12345" |> parse in *)
  blocks |> pt01 |> Printf.printf "pt01: %i\n%!" ;
