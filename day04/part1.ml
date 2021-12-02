let md5 str = str |> Digest.string |> Digest.to_hex

exception Predicate_match of int

let first ~f seq =
  try
    Seq.iter (fun e -> if f e then raise (Predicate_match e)) seq;
    None
  with Predicate_match v -> Some v

let nat =
  let open Seq in
  let rec nat' start () = Cons (start, nat' (start + 1)) in
  nat' 0

let test_candidate key n =
  Printf.sprintf "%s%d" key n |> md5 |> String.starts_with ~prefix:"00000"

let () =
  let key = Seq.fold_left ( ^ ) "" Aoc.stdin in
  match first ~f:(test_candidate key) nat with
  | Some v -> Printf.printf "%d\n" v
  | None -> print_endline "nothing???"
