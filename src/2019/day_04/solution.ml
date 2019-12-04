(* create an inclusive range of ints *)
let rangei a b =
  let rec imp l i =
    if i < a then l else imp (i::l) (i-1)
  in
  imp [] b



let validate_pt1 i =
  let rec imp prev had_matching = function
    | 0 -> had_matching
    | n ->
      let digit = n mod 10 in
      if digit > prev then false
      else imp digit (if digit = prev then true else had_matching) (n / 10)
  in
  imp (i mod 10) false (i / 10)



let validate_pt2 i =
  (* 
    p:  previous digit
    x:  repeated digits so far
    x': whether p was removed from x
  *)
  let rec imp p x x' = function
    | 0 -> x <> []
    | n ->
      let (d, n) = (n mod 10, n / 10) in
      if d > p then false
      else if x' then
        imp d x (if d = p then x' else false) n
      else if d = p && (x = [] || d <> (List.hd x)) then
        imp d (d::x) false n
      else if d = p && d = (List.hd x) then
        imp d (List.tl x) true n
      else
        imp d x false n
  in
  imp (i mod 10) [] false (i / 10)



let () =
  let input = rangei 178416 676461 in

  let run t f =
    input |> List.filter f
    |> List.length |> Printf.printf "%s: %i\n" t
  in

  run "Part1" validate_pt1 ;
  run "Part2" validate_pt2 ;

  ()
