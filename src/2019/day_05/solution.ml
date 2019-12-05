let read_lines filename =
  let file = open_in filename in
  let rec imp acc = 
    try imp (input_line file :: acc)
    with End_of_file -> close_in file; List.rev acc
  in
  imp []


type opcode = Add | Mul | In | Out | Jnz | Jz | Lt | Eq | Halt
type param_mode = Pos | Imm

let param_mode_from_int = function
  | 0 -> Pos
  | 1 -> Imm
  | x -> invalid_arg (Printf.sprintf "%i" x)

let opcode_from_int = function
  | 1 -> Add
  | 2 -> Mul
  | 3 -> In
  | 4 -> Out
  | 5 -> Jnz
  | 6 -> Jz
  | 7 -> Lt
  | 8 -> Eq
  | 99 -> Halt
  | x -> invalid_arg (Printf.sprintf "%i" x)

let opcode_to_int = function
  | Add -> 1
  | Mul -> 2
  | In  -> 3
  | Out -> 4
  | Jnz -> 5
  | Jz -> 6
  | Lt -> 7
  | Eq -> 8
  | Halt -> 99


(* decode an encoded instruction *)
let decode (i: int): (opcode * param_mode list) =
  let opcode = opcode_from_int ((i mod 10) + 10 * ((i / 10) mod 10)) in
  let rec get_pms acc x = function
    | 0 -> List.rev acc
    | n -> get_pms ((param_mode_from_int (x mod 10)) :: acc) (x / 10) (n-1)
  in
  opcode, (get_pms [] (i / 100) 3)



let run program input =
  let mem = Array.of_list program in

  let rec eval pc =
    let (op, param_modes) as inst = decode mem.(pc) in
    let ld_op i =
      let value = mem.(pc+i+1) in
      match List.nth param_modes i with
      | Imm -> value
      | Pos -> mem.(value)
    in

    match op with
    | Halt -> mem.(0)
    | In ->
      mem.(mem.(pc+1)) <- input ;
      eval (pc+2)
    | Out ->
      ld_op 0 |> Printf.printf "[Out] %i\n" ;
      eval (pc+2)
    | Jnz | Jz ->
      let op0 = ld_op 0 in
      let op1 = ld_op 1 in
      let f: (int -> int -> bool) = if op = Jnz then (<>) else (=) in
      eval (if f op0 0 then op1 else pc+3)
    | Add | Mul | Lt | Eq ->
      let op0 = ld_op 0 in
      let op1 = ld_op 1 in
      let dst = mem.(pc+3) in
      let f: (int -> int -> int) = match op with
      | Add -> (+) | Mul -> ( * )
      | Lt -> (fun a b -> a < b |> Bool.to_int)
      | Eq -> (fun a b -> a = b |> Bool.to_int)
      | _ -> failwith "unreachable"
      in
      mem.(dst) <- f op0 op1 ;
      eval (pc+4)
  in
  eval 0



let () =
  let program = read_lines "input.txt" |> List.hd |> String.split_on_char ',' |> List.map int_of_string in

  let _ = run program 1 in
  let _ = run program 5 in

  ()
