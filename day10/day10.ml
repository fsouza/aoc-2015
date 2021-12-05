open StdLabels
open MoreLabels

let new_acc = Printf.sprintf "%s%d%c"

let look_and_say str =
  let length = String.length str in
  let rec loop acc idx count curr =
    if idx >= length then new_acc acc count curr
    else
      let ch = str.[idx] in
      if Char.equal ch curr then loop acc (idx + 1) (count + 1) curr
      else loop (new_acc acc count curr) (idx + 1) 1 ch
  in
  loop "" 1 1 str.[0]

let rec look_and_say_seq str () =
  let next = look_and_say str in
  Seq.Cons (next, look_and_say_seq next)

let rec take n seq () =
  let open Seq in
  if n = 0 then Nil
  else
    match seq () with
    | Nil -> Nil
    | Cons (v, seq) -> Cons (v, take (n - 1) seq)

let run cache times str =
  match Hashtbl.find_opt cache str with
  | Some result -> result
  | None ->
      let result =
        str
        |> look_and_say_seq
        |> take times
        |> Seq.fold_left (fun _ el -> el) ""
        |> String.length
      in
      Hashtbl.replace ~key:str ~data:result cache;
      result

let () =
  let times = Sys.argv.(1) |> int_of_string in
  let cache = Hashtbl.create times in
  read_line ()
  |> String.to_seq
  |> Seq.map (String.make 1)
  |> Seq.map (run cache times)
  |> Seq.fold_left ( + ) 0
  |> Printf.printf "%d\n"
