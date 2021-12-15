open StdLabels

type register = A | B

type instruction =
  | Hlf of register
  | Tpl of register
  | Inc of register
  | Jmp of int
  | Jie of register * int
  | Jio of register * int

type state = { registers : int * int; ip : int }

let initial_state = { registers = (0, 0); ip = 0 }
let parse_offset = int_of_string

let parse_register = function
  | "a" -> Some A
  | "b" -> Some B
  | _ -> None

let parse line =
  match String.split_on_char ~sep:' ' line with
  | [ "hlf"; register ] ->
      parse_register register |> Option.map (fun register -> Hlf register)
  | [ "tpl"; register ] ->
      parse_register register |> Option.map (fun register -> Tpl register)
  | [ "inc"; register ] ->
      parse_register register |> Option.map (fun register -> Inc register)
  | [ "jmp"; offset ] -> Some (Jmp (parse_offset offset))
  | [ "jie"; register_with_comma; offset ] ->
      register_with_comma
      |> String.sub ~pos:0 ~len:1
      |> parse_register
      |> Option.map (fun register -> Jie (register, parse_offset offset))
  | [ "jio"; register_with_comma; offset ] ->
      register_with_comma
      |> String.sub ~pos:0 ~len:1
      |> parse_register
      |> Option.map (fun register -> Jio (register, parse_offset offset))
  | _ -> None

let execute { registers = (a_register, b_register) as registers; ip } = function
  | Hlf A -> { registers = (a_register / 2, b_register); ip = ip + 1 }
  | Hlf B -> { registers = (a_register, b_register / 2); ip = ip + 1 }
  | Tpl A -> { registers = (a_register * 3, b_register); ip = ip + 1 }
  | Tpl B -> { registers = (a_register, b_register * 3); ip = ip + 1 }
  | Inc A -> { registers = (a_register + 1, b_register); ip = ip + 1 }
  | Inc B -> { registers = (a_register, b_register + 1); ip = ip + 1 }
  | Jmp i -> { registers; ip = ip + i }
  | Jie (A, i) ->
      let offset = if a_register mod 2 = 0 then i else 1 in
      { registers; ip = ip + offset }
  | Jie (B, i) ->
      let offset = if b_register mod 2 = 0 then i else 1 in
      { registers; ip = ip + offset }
  | Jio (A, i) ->
      let offset = if a_register = 1 then i else 1 in
      { registers; ip = ip + offset }
  | Jio (B, i) ->
      let offset = if b_register = 1 then i else 1 in
      { registers; ip = ip + offset }

let rec run ({ ip; _ } as state) instructions =
  if ip = Array.length instructions then state
  else
    let instruction = instructions.(ip) in
    run (execute state instruction) instructions

let () =
  let instructions = Aoc.stdin |> Seq.filter_map parse |> Array.of_seq in
  instructions
  |> run initial_state
  |> fun { registers = a, b; _ } ->
  Printf.printf "Part 1: a=%d, b=%d\n" a b;
  instructions
  |> run { initial_state with registers = (1, 0) }
  |> fun { registers = a, b; _ } -> Printf.printf "Part 2: a=%d, b=%d\n" a b
