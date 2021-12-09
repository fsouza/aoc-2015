open StdLabels
open MoreLabels

module PosMap = Map.Make (struct
  type t = int * int

  let compare (row1, col1) (row2, col2) =
    let row = Int.compare row1 row2 in
    if row = 0 then Int.compare col1 col2 else row
end)

type pos = int * int
type range = pos * pos
type operation = Toggle of range | Turn_on of range | Turn_off of range

let all_positions ((start_row, start_col), (end_row, end_col)) =
  let open Seq in
  let rec pos_seq' (row, col) () =
    if row > end_row then Nil
    else
      let next =
        if col < end_col then (row, col + 1) else (row + 1, start_col)
      in
      Cons ((row, col), pos_seq' next)
  in
  pos_seq' (start_row, start_col)

let parse_pos pos =
  match String.split_on_char ~sep:',' pos with
  | [ row; col ] -> (int_of_string row, int_of_string col)
  | _ -> pos |> Printf.sprintf "invalid pos '%s'" |> invalid_arg

let parse line =
  match String.split_on_char ~sep:' ' line with
  | [ "toggle"; pos1; "through"; pos2 ] ->
      Some (Toggle (parse_pos pos1, parse_pos pos2))
  | [ "turn"; "on"; pos1; "through"; pos2 ] ->
      Some (Turn_on (parse_pos pos1, parse_pos pos2))
  | [ "turn"; "off"; pos1; "through"; pos2 ] ->
      Some (Turn_off (parse_pos pos1, parse_pos pos2))
  | _ -> None

let add_brightness state range delta =
  range
  |> all_positions
  |> Seq.fold_left
       (fun state pos ->
         let brightness =
           state |> PosMap.find_opt pos |> Option.value ~default:0
         in
         let new_brightness = brightness + delta in
         if new_brightness > 0 then
           PosMap.add ~key:pos ~data:new_brightness state
         else PosMap.remove pos state)
       state

let execute state = function
  | Turn_on range -> add_brightness state range 1
  | Turn_off range -> add_brightness state range (-1)
  | Toggle range -> add_brightness state range 2

let () =
  Aoc.stdin
  |> Seq.filter_map parse
  |> Seq.fold_left execute PosMap.empty
  |> PosMap.fold ~f:(fun ~key:_ ~data acc -> data + acc) ~init:0
  |> Printf.printf "%d\n"
