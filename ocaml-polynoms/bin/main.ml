open Polynoms

let () = print_endline "Hello, World!"
let () =
  let result = add 2 3 in
  print_endline (string_of_int result);
  let result = sub 3 1 in
  print_endline (string_of_int result)
