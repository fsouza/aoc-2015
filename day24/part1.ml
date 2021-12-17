open StdLabels
open MoreLabels
module IntSet = Set.Make (Int)

let qe = List.fold_left ~init:1 ~f:( * )

let make_first_group target =
  let rec make_first_group' sum items = function
    | _ when sum = target -> Some items
    | [] -> None
    | hd :: tl -> (
        let with_hd = make_first_group' (sum + hd) (hd :: items) tl in
        let without_hd = make_first_group' sum items tl in
        match (with_hd, without_hd) with
        | None, None -> None
        | Some v, None -> Some v
        | None, Some v -> Some v
        | Some items_with_hd, Some items_without_hd ->
            let hd_length = List.length items_with_hd in
            let nohd_length = List.length items_without_hd in
            if hd_length < nohd_length then Some items_with_hd
            else if hd_length > nohd_length then Some items_without_hd
            else
              let hd_qe = qe items_with_hd in
              let nohd_qe = qe items_without_hd in
              if hd_qe < nohd_qe then Some items_with_hd
              else Some items_without_hd)
  in
  make_first_group' 0 []

let () =
  let packages =
    Aoc.stdin
    |> Seq.map int_of_string
    |> List.of_seq
    |> List.sort ~cmp:(Fun.flip Int.compare)
  in
  let package_sum = packages |> List.fold_left ~init:0 ~f:( + ) in
  packages
  |> make_first_group (package_sum / 3)
  |> Option.value ~default:[]
  |> qe
  |> Printf.printf "Part 1: %d\n";
  packages
  |> make_first_group (package_sum / 4)
  |> Option.value ~default:[]
  |> qe
  |> Printf.printf "Part 2: %d\n"
