type t = int

let of_int v = v land 0xffff

let of_string v = v |> int_of_string |> of_int

let to_string = string_of_int

let b_and x y = x land y |> of_int

let b_or x y = x lor y |> of_int

let lshift x y = x lsl y |> of_int

let rshift x y = x lsr y |> of_int

let not x = lnot x |> of_int
