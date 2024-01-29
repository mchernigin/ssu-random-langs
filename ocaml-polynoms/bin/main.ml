module IntCoefficient = struct
  type t = int

  let compare = compare
  let abs = abs
  let add = ( + )
  let mul = ( * )
  let div = ( / )
  let negate = ( ~- )
  let to_string = string_of_int
  let of_float = int_of_float
end

module IntPolynom = Polynoms.Make (IntCoefficient)

let p1 = IntPolynom.of_list [ -1; 2; -3 ]
let () = p1 |> IntPolynom.to_string |> print_endline
let p2 = IntPolynom.of_list [ -8; 3; -1; 7; -9; 100 ]
let () = p2 |> IntPolynom.to_string |> print_endline
let sum = IntPolynom.add p1 p2
let () = sum |> IntPolynom.to_string |> print_endline
let () = sum |> IntPolynom.negate |> IntPolynom.to_string |> print_endline
let diff = IntPolynom.sub p2 p1
let () = sum |> IntPolynom.to_string |> print_endline
let () = diff |> IntPolynom.to_string |> print_endline
let v1 = IntPolynom.value p1 3
let () = v1 |> string_of_int |> print_endline
