module StringSet = Set.Make(String) ;;
module StringMap = Map.Make(String) ;;
module StringHashtbl = Hashtbl.Make(String) ;;


let read_lines filename =
  let file = open_in filename in
  let rec imp acc = 
    try imp (input_line file :: acc)
    with End_of_file -> close_in file; List.rev acc
  in
  imp []
;;

let (>>) f g x = g (f x) ;;

let rec any f = function
  | [] -> false
  | x::xs -> f x || any f xs
;;

let take n l =
  let rec imp acc = function
    | (0, _) -> List.rev acc
    | (_, []) -> List.rev acc
    | (n, x::xs) -> imp (x::acc) (n-1, xs)
  in imp [] (n, l)
;;

let str_drop n s =
  if n < 0 then s
  else if n > String.length s then ""
  else String.sub s n (String.length s - n)
;;

let str_split_on_first_occurrence c s =
  let idx = String.index s c in
  (String.sub s 0 idx, str_drop (idx+1) s)
;;




type module_type = Broadcaster | FlipFlop | Conjunction ;;

(* name, type, outputs *)
type module_definition = string * module_type * string list ;;



let parse_module s =
  let ty = match s.[0] with
    | '%' -> FlipFlop
    | '&' -> Conjunction
    | _   -> Broadcaster
  in
  let name, s = str_split_on_first_occurrence ' ' s in
  let name = if List.mem name.[0] ['%'; '&'] then str_drop 1 name else name in
  assert (String.starts_with ~prefix:"-> " s) ;
  let s = str_drop 3 s in
  let outputs = s |> String.split_on_char ',' |> List.map String.trim in
  (name, ty, outputs)
;;





type pulse = Low | High ;;
type module_state = FlipFlop of bool | Conjunction of pulse StringMap.t | Broadcaster | Button ;;

let flip_pulse = function Low -> High | High -> Low ;;





let get_inputs module_defs name =
  module_defs |> StringMap.filter (fun _ (_, _, outputs) -> List.mem name outputs) |> StringMap.bindings |> List.map fst
;;



let run module_defs =
  let modules = StringHashtbl.create 17 in
  module_defs |> StringMap.iter (fun _ (name, (ty: module_type), outputs) ->
    StringHashtbl.add modules name (match ty with
      | Broadcaster -> Broadcaster
      | FlipFlop -> FlipFlop false
      | Conjunction ->
        let inputs = get_inputs module_defs name in
        Conjunction (inputs |> List.map (fun n -> (n, Low)) |> StringMap.of_list)
    )
  ) ;
  StringHashtbl.add modules "button" Button ;
  let recv_only_modules = (
    let all_sending_modules = StringMap.fold (fun k _ acc -> StringSet.add k acc) module_defs StringSet.empty in
    let all_receiving_modules = StringMap.fold (fun _ (_, _, outputs) acc -> StringSet.union acc (StringSet.of_list outputs) ) module_defs StringSet.empty in
    StringSet.diff all_receiving_modules all_sending_modules
  ) in
  recv_only_modules |> StringSet.iter (fun name ->
    if not (StringHashtbl.mem modules name) then begin
      (* We add recv-only states (ie, states that don't ever send anything, but do receive inputs) as Conjunctions,
        so that we can keep track of the last pulses they've been receiving *)
        StringHashtbl.add modules name (Conjunction StringMap.empty)
    end
  ) ;
  let num_low_pulses = ref 0 in
  let num_high_pulses = ref 0 in
  let pt01_result = ref 0 in
  let pt02_result = ref 0 in
  let n = ref 0 in

  let queue = Queue.create () in
  let update_module_state name f =
    match StringHashtbl.find_opt modules name with
      | None -> StringHashtbl.add modules name (f None)
      | Some s -> StringHashtbl.replace modules name (f (Some s))
  in
  let schedule_pulse_to_outputs name pulse (expected_module_type: module_type) =
    match StringMap.find name module_defs with
      | (_, ty, outputs) when ty = expected_module_type -> begin
          assert (ty = expected_module_type) ;
          outputs |> List.iter (fun name' -> Queue.add (name, pulse, name') queue) ;
          match pulse with
            | Low -> num_low_pulses := !num_low_pulses + (List.length outputs)
            | High -> num_high_pulses := !num_high_pulses + (List.length outputs) ;
          ()
      end
      | _ -> failwith "unexpected module type"
  in
  let zh_num_cycles = StringHashtbl.create 4 in
  let inputs_to_zh = module_defs |> StringMap.filter (fun _ (name, _, outputs) -> List.mem "zh" outputs) |> StringMap.bindings |> List.map fst in
  inputs_to_zh |> List.iter (fun name -> StringHashtbl.add zh_num_cycles name 0) ;
  
  while (!pt01_result = 0) || (!pt02_result = 0) do
    incr n ;
    Queue.add ("", Low, "button") queue ;
    while not (Queue.is_empty queue) do
      let (sender, pulse, dest) = Queue.take queue in
      if StringSet.mem sender recv_only_modules then () else
      begin
        if dest = "zh" && pulse = High && StringHashtbl.find zh_num_cycles sender = 0 then
          StringHashtbl.replace zh_num_cycles sender !n ;
        if StringHashtbl.fold (fun _ v acc -> acc && v > 0) zh_num_cycles true then
          pt02_result := StringHashtbl.fold (fun _ i acc -> i*acc) zh_num_cycles 1 ;
        update_module_state dest (fun state -> match state with
          | None ->
            failwith (Printf.sprintf "unknown module '%s'" dest)
          | Some Button ->
            incr num_low_pulses ;
            Queue.add (dest, Low, "broadcaster") queue ;
            Button
          | Some Broadcaster -> schedule_pulse_to_outputs dest pulse Broadcaster ; Broadcaster
          | Some (FlipFlop state) -> begin
            match (pulse, state) with
              | High, _ -> FlipFlop state
              | Low, false -> schedule_pulse_to_outputs dest High FlipFlop ; FlipFlop (not state)
              | Low, true  -> schedule_pulse_to_outputs dest Low FlipFlop ; FlipFlop (not state)
            end
          | Some (Conjunction memory) ->
            let memory = StringMap.add sender pulse memory in
            let pulse_to_send = if StringMap.for_all (fun _ pulse -> pulse = High) memory then Low else High in
              schedule_pulse_to_outputs dest pulse_to_send Conjunction ;
              Conjunction memory
        ) ;
      end
    done ;
    if !n = 1000 then
      pt01_result := !num_low_pulses * !num_high_pulses ;
  done ;
  (!pt01_result, !pt02_result)
;;




let () =
  let modules = read_lines "input.txt"
    |> List.filter (String.starts_with ~prefix:"#" >> not)
    |> List.map parse_module in
  let modules = ("rx", (let ff: module_type = Conjunction in ff), []) :: modules in
  let module_defs = modules |> List.fold_left (fun acc ((n, _, _) as m) -> StringMap.add n m acc) StringMap.empty in
  let pt01, pt02 = run module_defs in
  Printf.printf "Pt01: %d\n%!" pt01 ;
  Printf.printf "Pt02: %d\n%!" pt02 ;

