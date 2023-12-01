type 'a list = Nil | Cons of 'a * 'a list


let rec count = function 
  | Nil -> 0
  | Cons (_, xs) -> 1 + (count xs)

let _ = Bool.to_int

let _ =
  Printf.printf "Count: %i\n" (count (Cons (1, Nil)))

