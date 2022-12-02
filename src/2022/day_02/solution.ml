let read_lines filename =
  let file = open_in filename in
  let rec imp acc = 
    try imp (input_line file :: acc)
    with End_of_file -> close_in file; List.rev acc
  in
  imp []

let compose f g = (fun x -> g (f x))


type shape = Rock | Paper | Scissors

let shape_of_char = function
    'A' | 'X' -> Rock
  | 'B' | 'Y' -> Paper
  | 'C' | 'Z' -> Scissors
  | _ -> failwith "Invalid input"


let score_of_shape = function
    Rock -> 1
  | Paper -> 2
  | Scissors -> 3


type result = Win | Draw | Loss

let result_of_char = function
  | 'X' -> Loss
  | 'Y' -> Draw
  | 'Z' -> Win
  | _ -> failwith ""

let result_of_round = function
    (Scissors, Rock) -> Win
  | (Paper, Scissors) -> Win
  | (Rock, Paper) -> Win
  | (h1, h2) when h1 = h2 -> Draw
  | _ -> Loss

let score_of_result = function
    Win -> 6
  | Draw -> 3
  | Loss -> 0

let score_of_round = compose result_of_round score_of_result

let eval_round hands = (score_of_round hands) + (snd hands |> score_of_shape)

let rec last = function
  | [] -> failwith ""
  | [x] -> x
  | x::xs -> last xs

let rec sum = function
  | [] -> 0
  | x::xs -> x + sum xs


let hand_for_win = function
  | Rock -> Paper
  | Paper -> Scissors
  | Scissors -> Rock

let hand_for_draw = function
  | Rock -> Rock
  | Paper -> Paper
  | Scissors -> Scissors

let hand_for_loss = function
  | Rock -> Scissors
  | Paper -> Rock
  | Scissors -> Paper

let hand_for_desired_result = function
  | Win -> hand_for_win
  | Draw -> hand_for_draw
  | Loss -> hand_for_loss



let () =
  let pt1_score = read_lines "input.txt"
    |> List.map (fun l -> (shape_of_char (String.get l 0), shape_of_char (String.get l 2)))
    |> List.map eval_round
    |> sum
  in Printf.printf "Pt 01: %i\n" pt1_score ;

  let pt2_score = read_lines "input.txt"
    |> List.map (fun l -> (shape_of_char (String.get l 0), result_of_char (String.get l 2)))
    |> List.map (fun (h, r) -> (h, hand_for_desired_result r h))
    |> List.map eval_round
    |> sum
  in Printf.printf "Pt 02: %i\n" pt2_score;