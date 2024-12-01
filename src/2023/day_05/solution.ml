module IntSet = Set.Make(Int)
module IntMap = Map.Make(Int)
module CatMap = Map.Make(String)


let read_lines filename =
  let file = open_in filename in
  let rec imp acc = 
    try imp (input_line file :: acc)
    with End_of_file -> close_in file; List.rev acc
  in
  imp []
;;


let rec has_prefix p l = match p, l with
  | [], _ -> true
  | _::_, [] -> false
  | p::ps, l::ls -> if p = l then has_prefix ps ls else false
;;

let rec skip_first_n n l = match n, l with
  | _, [] -> []
  | 0, _ -> l
  | n, l::ls -> skip_first_n (n-1) ls
;;


let prefix_while p l =
  let rec imp acc = function
    | [] -> (acc, [])
    | x::xs -> if p x then imp (acc@[x]) xs else (acc, x::xs)
  in imp [] l
;;

let rec skip_while p = function
  | [] -> []
  | x::xs when p x -> skip_while p xs
  | xs -> xs
;;


let opt_get = function
  | Some a -> a
  | None -> failwith "None"
;;


let comp f g = fun x -> f (g x) ;;

let (^~) = comp ;;


let rec list_min = function
  | [] -> failwith "[]"
  | [x] -> x
  | x::xs -> min x (list_min xs)
;;


let last l = List.nth l (List.length l - 1) ;;

let seq_min s = match Seq.uncons s with
  | None -> failwith "Empty"
  | Some (e, s) -> Seq.fold_left (min) e s
;;


(* let split_list' pattern l =
  let rec imp acc cur = function
    | [] -> acc
    | l -> if has_prefix pattern l then imp (acc @ )
  in
;; *)



let parse_range line =
  let split = line |> String.split_on_char ' ' in
  if List.length split <> 3 then None
  else match List.map int_of_string_opt split with
    | [Some dst_start; Some src_start; Some len] -> Some (dst_start, src_start, len)
    | _ -> None
;;


let parse_input (parse_seeds: string -> int Seq.t) (input: string list) =
  let seeds = parse_seeds (List.hd input) in

  let input = skip_first_n 2 input in

  let rec parse_blocks acc l = match l with
    | [] -> acc
    | l::ls ->
      let name = String.split_on_char ' ' l |> List.hd in
      Printf.printf "NAME: %s\n" name ;
      let lines, rest = prefix_while (fun s -> s <> "") ls in
      let ranges = List.map (opt_get ^~ parse_range) lines in
      parse_blocks (CatMap.add name ranges acc) (skip_while (fun s -> s = "") rest)
  in

  let blocks = parse_blocks CatMap.empty input in

  (* let xxx = List.fold_left2 (fun acc line next -> (
    (* Printf.printf "\nline: %s\nnext: %s\n" line next ; *)
    []
  )) [] input ((List.tl input) @ [":EOL:"]) in
  [] *)
  (seeds, blocks)
;;


let rec map_number n = function
  | [] -> n (* if we've exhausted all ranges, and none matched the number, we apply the ideitity mapping *)
  | (dst_start, src_start, len)::rs ->
    if src_start <= n && (src_start+len) > n
    then dst_start + (n - src_start)
    else map_number n rs
;;

let seed_to_loc (cache: int IntMap.t) cats cur_cat n: (int IntMap.t * int) =
  let rec imp cur_cat n =
    match CatMap.filter (fun k _ -> String.starts_with ~prefix:cur_cat k) cats |> CatMap.choose_opt with
      | None -> n
      | Some (name, mapping) ->
        let n = map_number n mapping in
        match String.split_on_char '-' name |> last with
          | "location" -> n
          | next_cat -> imp next_cat n
  in
  match IntMap.find_opt n cache with
    | Some n -> cache, n
    | None -> let res = imp cur_cat n in (IntMap.add n res cache, res)
;;



let dump_caches caches =
  Printf.printf "CACHE: (#=%i) \n" (CatMap.cardinal caches) ;
  CatMap.iter (fun name cache ->
    if IntMap.is_empty cache
    then Printf.printf "cat[%s]: {}\n" name
    else IntMap.iter (Printf.printf "cat[%s]: %i -> %i\n" name) cache
    ) caches;
  Printf.printf "CACHE END\n" ;
;;

let cat_names m = CatMap.fold (fun k _ acc -> k::acc) m [] ;;




let seed_to_loc' (caches: int IntMap.t CatMap.t) cats cur_cat n: (int IntMap.t CatMap.t * int) =
  (* Printf.printf "seed_to_loc'(%i)\n" n ; *)
  let rec imp caches cur_cat n =
    (* Printf.printf "seed_to_loc'.imp(%s, %i)\n" cur_cat n ; *)
    match CatMap.filter (fun k _ -> String.starts_with ~prefix:cur_cat k) cats |> CatMap.choose_opt with
      (*| None -> (caches, n) (* TODO do we ever end up in here? *) *)
      | None -> failwith "test?"
      | Some (name, mapping) ->
        match IntMap.find_opt n (CatMap.find name caches) with
          (* Found a cached res, return that *)
          | Some res -> Printf.printf "CACHE HIT\n%!" ; (caches, res)
          | None -> (
            (* Printf.printf "CACHE MISS\n%!" ; *)
            (* let update_caches res caches = CatMap.add name (IntMap.add n res (CatMap.find name caches)) caches in *)
            (* let update_caches res =
              Printf.printf "CACHE UPDATE\n" ;
              let c = CatMap.update name (fun cache -> Some (IntMap.add n res (opt_get cache))) caches
              in
              dump_caches c ; c
            in *)
            let loc = map_number n mapping in
            (* let caches = CatMap.add name (IntMap.add n loc (CatMap.find name caches)) caches in *)
            match String.split_on_char '-' name |> last with
              | "location" -> (caches, loc)
              (* | next_cat -> let (caches, res) = imp caches next_cat n in (CatMap.add name (IntMap.add ) caches) *)
              | next_cat ->
                let (caches, res) = imp caches next_cat loc
                in (caches, res)
          )
  in
  match CatMap.filter (fun k _ -> String.starts_with ~prefix:cur_cat k) caches |> CatMap.choose_opt with
    | None -> failwith "no cache found"
    | Some (_, cache) ->
      let update_caches res caches =
        cat_names cats
          |> List.fold_left (fun caches name -> CatMap.update name (fun cache -> Some (IntMap.add n res (opt_get cache))) caches) caches
          (* caches *)
      in
      match IntMap.find_opt n cache with
        | Some res -> caches, res
        | None -> let (caches', res) = imp caches cur_cat n in (update_caches res caches, res)
;;



let run parse_range input =
  let seeds, cats = parse_input parse_range input in
  seeds
    |> Seq.fold_left (fun (cache, cur_min) seed ->
      let (cache, loc) = seed_to_loc cache cats "seed" seed in
      (cache, match cur_min with None -> Some loc | Some loc' -> Some (min loc loc'))
    ) (IntMap.empty, None)
    |> snd |> opt_get
;;



let run' parse_range input =
  let seeds, cats = parse_input parse_range input in
  Printf.printf "DID PARSE SEEDS\n%!" ;
  let (caches, res) = seeds
    |> Seq.fold_left (fun (caches, cur_min) seed ->
      let (caches', loc) = seed_to_loc' caches cats "seed" seed in
      (* Printf.printf "seed %i -> loc %i\n" seed loc ; *)
      (* if caches <> caches' then (Printf.printf "CACHES UPDATED\n" ; dump_caches caches' ) ; *)
      (caches', match cur_min with None -> Some loc | Some loc' -> Some (min loc loc'))
    ) (CatMap.of_list (cat_names cats |> List.map (fun n -> (n, IntMap.empty))), None)
    in
    Printf.printf "CACHE: (#=%i) \n" (CatMap.cardinal caches) ;
    dump_caches caches ;
    (* CatMap.iter (fun name cache -> IntMap.iter (Printf.printf "cat[%s]: %i -> %i\n" name) cache) caches; *)
    Printf.printf "CACHE END\n" ;
    res |> opt_get
;;



let pt01 =
  Printf.printf "Running pt01\n" ;
  (* let seeds, cats = parse_input (fun str -> 
    str |> String.split_on_char ' ' |> List.filter_map int_of_string_opt |> List.to_seq
  ) input in
  (* let fst_seed, seeds = seeds |> Seq.uncons |> opt_get in *)
  seeds
    (* |> Seq.map (seed_to_loc cats "seed") *)
    |> Seq.fold_left (fun (cache, cur_min) seed ->
      let (cache, loc) = seed_to_loc cache cats "seed" seed in
      (cache, match cur_min with None -> Some loc | Some loc' -> Some (min loc loc'))
    ) (IntMap.empty, None)
;; *)
  run' (fun str ->
    str |> String.split_on_char ' ' |> List.filter_map int_of_string_opt |> List.to_seq
  )
;;



let rec dump_ranges = function
  | [] -> ()
  | (start, len)::rs ->
    Printf.printf "(%i, %i)%s" start len (if rs = [] then "" else "; ") ;
    dump_ranges rs
;;


let insert_at idx elem xs =
  let rec imp cur_idx = function
    | [] -> if cur_idx = idx then [elem] else failwith  (Printf.sprintf "invalid index %i (cur_idx: %i)" idx cur_idx)
    | x::xs ->
      if cur_idx = idx then elem::x::xs
      else x::(imp (cur_idx+1) xs)
  in imp 0 xs
;;

let cmp_range_starts (start,_) (start',_) = start - start' ;;

type comparison_result = EQ | GT | LT


let is_sorted f l = List.sort f l = l ;;

let as_comparison_result x =
  if x < 0 then GT
  else if x = 0 then EQ
  else LT
;;


let process_ranges ranges =
  let rec imp = function
    | [] -> []
    | [r] -> [r]
    | (((start, len) as r)::((start', len') as r')::rs) ->
      if start+len >= start'
      then imp ((start, start'+len' - start)::rs)
      else r::(imp (r'::rs))
  in imp (List.sort_uniq (cmp_range_starts) ranges)
;;


let rec insert_range ((start, len) as r) = function
  | [] -> [r]
  | rs ->
    let idx = match List.find_index (fun r' -> cmp_range_starts r' r >= 0) rs with Some idx -> idx | None -> List.length rs in
    insert_at idx r rs |> process_ranges
;;


let rec check_ranges_valid = function
  | [] -> true
  | [_] -> true
  | ((start, len)::((start', len') as r')::rs) -> start < start' && start+len < start' && check_ranges_valid (r'::rs)
;;


let pt02 =
  (* Printf.printf "Parsing input (pt02)\n%!" ;
  let seeds, cats = parse_input (fun s ->
    (* s |> String.split_on_char ' ' |> List.filter_map int_of_string_opt |> IntSet.of_list *)
    
    let numbers = s |> String.split_on_char ' ' |> List.filter_map int_of_string_opt in

    let rec imp acc = function
      | [] -> acc
      | x1::x2::xs -> imp (Seq.append acc (Seq.unfold (fun x -> if x < x1+x2 then Some (x, x+1) else None) x1)) xs
    in imp Seq.empty numbers


    (* let rec mk_range acc s = function
      | 0 -> acc
      | n -> mk_range (IntSet.union acc (IntSet.singleton s)) (s+1) (n-1)
    in
    let rec imp acc = function
      | [] -> acc
      | _::len::xs -> imp (acc + len) xs
    in
    let total = imp 0 numbers in Printf.printf "total: %i\n%!" total ;
    let rec imp acc = function
      | [] -> acc
      | x1::x2::xs -> Printf.printf "making range for %i:%i\n%!" x1 x2 ; imp (mk_range IntSet.empty x1 x2 |> IntSet.union acc) xs
    in imp IntSet.empty numbers *)
  ) input in
  Printf.printf "Running seed_to_loc (pt02)\n%!" ;
  seeds
    |> Seq.map (seed_to_loc cats "seed")
    |> seq_min
;; *)


  run' (fun s ->
    (* s |> String.split_on_char ' ' |> List.filter_map int_of_string_opt |> IntSet.of_list *)
    
    let numbers = s |> String.split_on_char ' ' |> List.filter_map int_of_string_opt in

    let total =
      let rec imp'' acc = function
        | [] -> acc
        | _::l::xs -> imp'' (acc+l) xs
    in imp'' 0 numbers in
    Printf.printf "TOTAL: %i\n" total ;

    let rec imp' acc = function
      | [] -> acc
      | x1::x2::xs -> imp' (insert_range (x1,x2) acc) xs
    in
    let ranges =  numbers |> imp' [] in
    ranges |> List.map snd |> List.fold_left (+) 0 |> Printf.printf "TOTAL: %i\n"  ;
    dump_ranges ranges ;
    assert (check_ranges_valid ranges) ;

    failwith "TODO" ;

    let rec imp acc = function
      | [] -> acc
      | x1::x2::xs -> imp (Seq.append acc (Seq.unfold (fun x -> if x < x1+x2 then Some (x, x+1) else None) x1)) xs
    in
    (* Printf.printf "AAAAAAAA\n" ;
    imp Seq.empty [0;4; 12;1; 14;0; 70;12] |> List.of_seq |> List.iter (Printf.printf " %i") ;
    Printf.printf "\nAAAAAAAA\n" ; *)
    imp Seq.empty numbers
  )
;;



let () =
  let input = read_lines "input.txt" in
  input |> pt01 |> Printf.printf "pt01: %i\n" ;
  input |> pt02 |> Printf.printf "pt02: %i\n" ;

