let read_lines filename =
  let file = open_in filename in
  let rec imp acc = 
    try imp (input_line file :: acc)
    with End_of_file -> close_in file; List.rev acc
  in
  imp []
;;



let o f g = fun x -> f (g x) ;;


let get_chars s =
  let rec imp l = function
    | -1 -> l
    | i  -> imp (s.[i] :: l) (i-1)
  in
  imp [] (String.length s - 1)
;;


let rec count_where f = function
  | [] -> 0
  | x::xs -> (if f x then 1 else 0) + count_where f xs
;;


let rec any f = function
  | [] -> false
  | x::xs -> if f x then true else any f xs
;;

let rec all f = function
  | [] -> true
  | x::xs -> if not (f x) then false else any f xs
;;


let num_occurrences e l = count_where (fun x -> x = e) l ;;


let inspect f l =
  let rec imp = function
    | [] -> ()
    | x::xs -> f x ; imp xs
  in imp l ;
  l
;;




type card = A | K | Q | J | T | N9 | N8 | N7 | N6 | N5 | N4 | N3 | N2
let all_cards = [A; K; Q; J; T; N9; N8; N7; N6; N5; N4; N3; N2] ;;

type hand = card * card * card * card * card

type cmp = EQ | GT | LT

let int_of_cmp = function
  | EQ -> 0
  | GT -> 1
  | LT -> -1
;;


type hand_type = FiveOfAKind | FourOfAKind | FullHouse | ThreeOfAKind | TwoPair | OnePair | HighCard



let list_of_hand (a, b, c, d, e) = [a; b; c; d; e] ;;
let hand_of_list [a; b; c; d; e] = (a, b, c, d, e) ;;



let nat_of_card = function
  | A -> 14 | K -> 13 | Q -> 12 | J -> 11 | T -> 10
  | N9 -> 9 | N8 -> 8 | N7 -> 7 | N6 -> 6 | N5 -> 5 | N4 -> 4 | N3 -> 3 | N2 -> 2
;;


let parse_card = function
  | 'A' -> A | 'K' -> K | 'Q' -> Q | 'J' -> J | 'T' -> T
  | '9' -> N9 | '8' -> N8 | '7' -> N7 | '6' -> N6 | '5' -> N5 | '4' -> N4 | '3' -> N3 | '2' -> N2
;;

let string_of_card = function
  | A -> "A" | K -> "K" | Q -> "Q" | J -> "J" | T -> "T"
  | N9 -> "9" | N8 -> "8" | N7 -> "7" | N6 -> "6" | N5 -> "5" | N4 -> "4" | N3 -> "3" | N2 -> "2"


let compare_cards a b =
  let a, b = (nat_of_card a, nat_of_card b) in
  if a = b then EQ
  else if a < b then LT
  else GT
;;




let int_of_hand_type = function
  | FiveOfAKind -> 6 | FourOfAKind -> 5 | FullHouse -> 4 | ThreeOfAKind -> 3
  | TwoPair -> 2 | OnePair -> 1 | HighCard -> 0
;;

let compare_hand_type a b =
  let a, b = (int_of_hand_type a, int_of_hand_type b) in
  if a = b then EQ
  else if a < b then LT
  else GT
;;




module CardSet = Set.Make(
struct
  type t = card
  let compare = (fun a b -> int_of_cmp (compare_cards a b))
end
)



let hand_type_of_hand hand =
  let set = CardSet.of_list (list_of_hand hand) in
  match CardSet.cardinal set with
    | 1 -> FiveOfAKind 
    | 4 -> OnePair
    | 5 -> HighCard
    | _ ->
      let has_of_count n = any (fun c -> (num_occurrences c (list_of_hand hand)) = n) all_cards in
      if has_of_count 4 then FourOfAKind
      else if has_of_count 3 && has_of_count 2 then FullHouse
      else if has_of_count 3 then ThreeOfAKind
      else TwoPair
;;


let compare_hands h1 h2 =
  match compare_hand_type (hand_type_of_hand h1) (hand_type_of_hand h2) with
    | GT -> GT
    | LT -> LT
    | EQ ->
      let rec imp = function
        | [] -> EQ
        | (c1,c2)::cs -> match compare_cards c1 c2 with GT -> GT | LT -> LT | EQ -> imp cs
      in imp (List.combine (list_of_hand h1) (list_of_hand h2))
;;


let rec parse_input = function
  | [] -> []
  | x::xs ->
    let hand, bid = (let x = x |> String.split_on_char ' ' in (List.nth x 0, List.nth x 1)) in
    let hand = hand |> get_chars |> List.map parse_card |> hand_of_list in
    (hand, int_of_string bid) :: (parse_input xs)
;;



let pt01 input =
  let dump_pair (hand, bid) =
    Printf.printf "(%s %i)\n" (hand |> list_of_hand |> List.map string_of_card |> String.concat "") bid
  in
  input
    |> parse_input
    (* |> inspect dump_pair *)
    |> List.sort (fun a b -> compare_hands (fst a) (fst b) |> int_of_cmp)
    |> inspect dump_pair
    |> List.mapi (fun idx (_, bid) -> (idx+1) * bid)
    |> List.fold_left (+) 0
;;




let pt02 input = 12 ;;



let () =
  let input = read_lines "input.txt" in
  input |> pt01 |> Printf.printf "Pt01: %i\n" ;
  input |> pt02 |> Printf.printf "Pt02: %i\n" ;
