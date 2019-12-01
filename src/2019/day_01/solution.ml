let read_lines filename =
  let file = open_in filename in
  let rec imp acc = 
    try imp (input_line file :: acc)
    with End_of_file -> close_in file; List.rev acc
  in
  imp []


let calc_fuel_pt1 mass =
  ((int_of_string mass) / 3) - 2

let calc_fuel_pt2 mass =
  let rec imp acc = function
    | x when x <= 0 -> acc
    | x ->
      let fuel = (x / 3) - 2 in
      imp (acc + (max 0 fuel)) fuel
  in
  imp 0 mass


let () =
  read_lines "input.txt"
  |> List.map calc_fuel_pt1
  |> List.fold_left (+) 0
  |> Printf.printf "Total (Pt 1): %i\n" ;

  read_lines "input.txt"
  |> List.map (fun x -> x |> int_of_string |> calc_fuel_pt2)
  |> List.fold_left (+) 0
  |> Printf.printf "Total (Pt 2): %i\n" ;