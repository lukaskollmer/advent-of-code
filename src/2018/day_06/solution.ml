let read_lines filename =
  let file = open_in filename in
  let rec imp acc = 
    try imp (input_line file :: acc)
    with End_of_file -> close_in file; List.rev acc
  in
  imp []



let point_of_string str =
  let split = String.split_on_char ',' str in
  let x = int_of_string (List.hd split) in
  let y = int_of_string (split |> List.tl |> List.hd |> String.trim) in
  (x, y)

let string_of_point (id, x, y) = Printf.sprintf "([%i] %i, %i)" id x y
let print_point p = p |> string_of_point |> print_endline



let rangei a b =
  let rec imp l i =
    if i < a then l else imp (i::l) (i-1)
  in
  imp [] b

let range a b = rangei a (b-1)


let count_where f l =
  let rec imp acc = function
    | [] -> acc
    | x::xs -> if f x then imp (acc+1) xs else imp acc xs
  in
  imp 0 l

let () =
  let input = read_lines "input.txt" in
  let points = List.mapi (fun i s -> let (x, y) = point_of_string s in (i, x, y)) input in

  let (width, height) = points
    |> List.fold_left (fun (w, h) (_, x, y) -> max w x, max h y) (0, 0)
    |> (fun (w, h) -> w+1, h+1) in

  let all_tiles = range 0 width |> List.map (fun y -> range 0 height |> List.map (fun x -> (x, y))) |> List.flatten in

  let d (x, y) (x', y') = (abs (x - x')) + (abs (y - y')) in


  let distances = Array.init height (fun _ -> Array.make width (-1, -1)) in

  all_tiles |> List.iter (fun (x, y) ->
    let nearest_points = points |> List.map (fun (id, x', y') -> id, d (x, y) (x', y')) |> List.sort (fun (_, d) (_, d') -> d - d') in
    distances.(y).(x) <- match nearest_points with
    | (id, distance) :: (id', distance') :: _ -> if distance = distance' then (-1, -1) else (id, distance)
    | _ -> failwith "should never reach here"
  ) ;

  (* part 1 *)

  let points_with_finite_fields = points |> List.filter (fun (id, x, y) ->
    let exists f1 f2 =
      points |> List.exists (fun (id', x', y') -> id <> id' && f1 x x' && f2 y y')
    in
    exists (<) (<) && exists (<) (>) && exists (>) (<) && exists (>) (>)
  ) in

  let distances_as_list = distances |> Array.map Array.to_list |> Array.to_list |> List.flatten in

  points_with_finite_fields
  |> List.map (fun (id, _, _) -> id, distances_as_list |> count_where (fun (id', _) -> id = id'))
  |> List.sort (fun (_, count) (_, count') -> count' - count)
  |> List.hd |> (fun (id, size) -> Printf.printf "[part 1] id: %i size: %i\n" id size) ;


  (* part 2 *)

  all_tiles
  |> List.filter (fun pos -> (points |> List.fold_left (fun acc (_, x', y') -> acc + d pos (x', y')) 0) < 10000)
  |> List.length |> Printf.printf "[part 2] count: %i" ;
