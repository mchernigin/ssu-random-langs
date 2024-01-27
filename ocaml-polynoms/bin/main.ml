module IntCoefficient =
  struct
    type t = int
    let to_string = string_of_int
  end

module IntPolynom =
  Polynoms.Make (IntCoefficient)

let p = IntPolynom.of_list [1; 2; 3]
let () = print_endline (IntPolynom.to_string p)
