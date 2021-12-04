open StdLabels
open MoreLabels
module StringMap = Map.Make (String)

type operator = And | Or | Lshift | Rshift

type operation =
  | Int of Int16.t
  | Var of string
  | Bin_op of operator * operation * operation
  | Not of operation

type instruction = { target : string; operation : operation }

let is_alpha =
  let is_alpha = function
    | 'a' .. 'z' -> true
    | 'A' .. 'Z' -> true
    | _ -> false
  in
  String.for_all ~f:is_alpha

let parse_value v = if is_alpha v then Var v else Int (Int16.of_string v)

let parse_operator = function
  | "AND" -> Some And
  | "OR" -> Some Or
  | "LSHIFT" -> Some Lshift
  | "RSHIFT" -> Some Rshift
  | _ -> None

let fn_of_operator = function
  | And -> Int16.b_and
  | Or -> Int16.b_or
  | Lshift -> Int16.lshift
  | Rshift -> Int16.rshift

let parse_operation op =
  match String.split_on_char ~sep:' ' op with
  | [ v ] -> Some (parse_value v)
  | [ "NOT"; v ] -> Some (Not (parse_value v))
  | [ lhs; op; rhs ] ->
      op
      |> parse_operator
      |> Option.map (fun operator ->
             Bin_op (operator, parse_value lhs, parse_value rhs))
  | _ -> None

let parse line =
  match Str.split (Str.regexp "->") line with
  | [ lhs; rhs ] ->
      let target = String.trim rhs in
      lhs
      |> String.trim
      |> parse_operation
      |> Option.map (fun operation -> { target; operation })
  | _ -> None

type state = { pending : operation StringMap.t; resolved : Int16.t StringMap.t }

let rec eval resolved = function
  | Int v -> Some v
  | Var v -> StringMap.find_opt v resolved
  | Not op -> op |> eval resolved |> Option.map Int16.not
  | Bin_op (operator, lhs, rhs) -> (
      match (eval resolved lhs, eval resolved rhs) with
      | Some lhs, Some rhs ->
          let fn = fn_of_operator operator in
          Some (fn lhs rhs)
      | _ -> None)

let execute ~key ~data { resolved; pending } =
  match eval resolved data with
  | Some v ->
      {
        resolved = StringMap.add ~key ~data:v resolved;
        pending = StringMap.remove key pending;
      }
  | None -> { resolved; pending }

let rec process ({ pending; _ } as state) =
  if StringMap.is_empty pending then state
  else pending |> StringMap.fold ~init:state ~f:execute |> process

let solve state =
  state |> process |> fun { resolved; _ } -> StringMap.find "a" resolved

let () =
  let initial_state =
    Aoc.stdin
    |> Seq.filter_map parse
    |> Seq.fold_left
         (fun ({ pending; _ } as state) { target; operation } ->
           {
             state with
             pending = StringMap.add ~key:target ~data:operation pending;
           })
         { pending = StringMap.empty; resolved = StringMap.empty }
  in
  let solution_part1 = solve initial_state in
  let state_part2 =
    {
      pending = StringMap.remove "b" initial_state.pending;
      resolved =
        StringMap.add ~key:"b" ~data:solution_part1 initial_state.resolved;
    }
  in
  let solution_part2 = solve state_part2 in
  Printf.printf "Part 1: %s\n" (Int16.to_string solution_part1);
  Printf.printf "Part 2: %s\n" (Int16.to_string solution_part2)
