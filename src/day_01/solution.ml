#use "topfind" ;;
#require "extlib" ;;


let input = Std.input_list (open_in "input.txt") |> List.map int_of_string


(* part 1 *)
let _ = input |> List.fold_left (+) 0 |> Printf.sprintf "part 1: %i" |> print_endline


(* part 2 *)

type tree = Leaf | Node of int * tree * tree

let rec bt_insert v = function
  | Leaf -> Node (v, Leaf, Leaf)
  | Node (v', l, r) ->
    if v < v' then Node (v', bt_insert v l, r)
    else Node (v', l, bt_insert v r)

let rec bt_contains v = function
  | Leaf -> false
  | Node (v', l, r) ->
    if v = v' then true
    else if v < v' then bt_contains v l
    else bt_contains v r

(*
Q: why the binary tree, instead of a list?
A: bt_contains is waaay faster than List.exists or List.mem: O(log n) vs O(n)

Q: Why does this matter?
A: part2 is called ~142k times, each time with a new element in `freq_history` 
   and every time we check whether it contains the current freq,
   which means, according to my calculations (bear in mind that i suck at math),
   that the list version needs to perform at least `1 + 2 + ... + 142k` comparisons,
   while the tree version need to perform just about `log (1 + 2 + ... + 142k)` comparisons (which is a lot less)

Performance:
- using a tree: ~5s
- using a list: ~250s
*)
let rec part2 freq freq_history = function
  | [] -> part2 freq freq_history input
  | v::input ->
    let freq = freq + v in
    if bt_contains freq freq_history then freq
    else part2 freq (bt_insert freq freq_history) input

let _ = part2 0 Leaf input |> Printf.sprintf "part 2: %i" |> print_endline