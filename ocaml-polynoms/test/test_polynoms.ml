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

let test title f = Alcotest.test_case title `Quick f

let () =
  Alcotest.run "polynoms"
    [
      ( "IntPolynom.of_list",
        [
          test "basic" (fun () ->
              let result =
                [ 1; 2; 3 ] |> IntPolynom.of_list |> IntPolynom.coefficients
              in
              let expected = [ 1; 2; 3 ] in
              Alcotest.(check (list int)) "" expected result);
          test "leading zeros" (fun () ->
              let result =
                [ 1; 2; 3; 0; 0; 0 ] |> IntPolynom.of_list
                |> IntPolynom.coefficients
              in
              let expected = [ 1; 2; 3 ] in
              Alcotest.(check (list int)) "" expected result);
          test "single zero" (fun () ->
              let result =
                [ 0 ] |> IntPolynom.of_list |> IntPolynom.coefficients
              in
              let expected = [ 0 ] in
              Alcotest.(check (list int)) "" expected result);
          test "empty" (fun () ->
              let result =
                [] |> IntPolynom.of_list |> IntPolynom.coefficients
              in
              let expected = [ 0 ] in
              Alcotest.(check (list int)) "" expected result);
        ] );
      ( "IntPolynom.to_string",
        [
          test "with negative" (fun () ->
              let p = IntPolynom.of_list [ -8; 3; -1; 7; -9; 100 ] in
              let result = IntPolynom.to_string p in
              let expected = "100x^5 - 9x^4 + 7x^3 - 1x^2 + 3x - 8" in
              Alcotest.(check string) "incorrect to_string" expected result);
          test "leading negative" (fun () ->
              let p = IntPolynom.of_list [ -8; 3; -1; 7; -9; -100 ] in
              let result = IntPolynom.to_string p in
              let expected = "- 100x^5 - 9x^4 + 7x^3 - 1x^2 + 3x - 8" in
              Alcotest.(check string) "incorrect to_string" expected result);
          test "empty" (fun () ->
              let p = IntPolynom.of_list [] in
              let result = IntPolynom.to_string p in
              let expected = "0" in
              Alcotest.(check string) "incorrect to_string" expected result);
          test "one element" (fun () ->
              let p = IntPolynom.of_list [ 1 ] in
              let result = IntPolynom.to_string p in
              let expected = "1" in
              Alcotest.(check string) "incorrect to_string" expected result);
          test "zeros" (fun () ->
              let p = IntPolynom.of_list [ 0; 1; 0; 2; 0; 3 ] in
              let result = IntPolynom.to_string p in
              let expected = "3x^5 + 2x^3 + x" in
              Alcotest.(check string) "incorrect to_string" expected result);
          test "final boss" (fun () ->
              let p = IntPolynom.of_list [ 0; 0; 1; 0; 2; 0; -3; 0; -0 ] in
              let result = IntPolynom.to_string p in
              let expected = "- 3x^6 + 2x^4 + x^2" in
              Alcotest.(check string) "incorrect to_string" expected result);
        ] );
      ( "IntPolynom.add",
        [
          test "basic" (fun () ->
              let p1 = IntPolynom.of_list [ 1; 2 ] in
              let p2 = IntPolynom.of_list [ 3; 4; 5 ] in
              let result = IntPolynom.add p1 p2 |> IntPolynom.coefficients in
              let expected = [ 4; 6; 5 ] in
              Alcotest.(check (list int)) "" expected result);
          test "with negative" (fun () ->
              let p1 = IntPolynom.of_list [ 1; 2; -3; 4; -5 ] in
              let p2 = IntPolynom.of_list [ -7; -1; 7; -3; -5 ] in
              let result = IntPolynom.add p1 p2 |> IntPolynom.coefficients in
              let expected = [ -6; 1; 4; 1; -10 ] in
              Alcotest.(check (list int)) "" expected result);
          test "opposite" (fun () ->
              let p1 = IntPolynom.of_list [ 1; 2; -3; 4; -5 ] in
              let p2 = IntPolynom.of_list [ -1; -2; 3; -4; 5 ] in
              let result = IntPolynom.add p1 p2 |> IntPolynom.coefficients in
              let expected = [ 0 ] in
              Alcotest.(check (list int)) "" expected result);
        ] );
      ( "IntPolynom.negate",
        [
          test "basic" (fun () ->
              let p = IntPolynom.of_list [ 0; 3; -4; 5; 0; -1; 5 ] in
              let result = IntPolynom.negate p |> IntPolynom.coefficients in
              let expected = [ 0; -3; 4; -5; 0; 1; -5 ] in
              Alcotest.(check (list int)) "" expected result);
        ] );
      ( "IntPolynom.value",
        [
          test "basic" (fun () ->
              let p = IntPolynom.of_list [ 0; -1; 7; 3; -26; 5; -20; 0; 25 ] in
              let result = IntPolynom.value p ~-3 in
              let expected = 146109 in
              Alcotest.(check int) "" expected result);
          test "zero" (fun () ->
              let p = IntPolynom.of_list [ 0; 3; -4; 5; 0; -1; 5 ] in
              let result = IntPolynom.value p 0 in
              let expected = 0 in
              Alcotest.(check int) "" expected result);
        ] );
      ( "IntPolynom.sub",
        [
          test "basic" (fun () ->
              let p1 = IntPolynom.of_list [ 1; 2 ] in
              let p2 = IntPolynom.of_list [ 3; 4; 5 ] in
              let result = IntPolynom.sub p1 p2 |> IntPolynom.coefficients in
              let expected = [ -2; -2; -5 ] in
              Alcotest.(check (list int)) "" expected result);
          test "with negative" (fun () ->
              let p1 = IntPolynom.of_list [ 1; 2; -3; 4; -5 ] in
              let p2 = IntPolynom.of_list [ -7; -1; 7; -3; -5 ] in
              let result = IntPolynom.sub p1 p2 |> IntPolynom.coefficients in
              let expected = [ 8; 3; -10; 7 ] in
              Alcotest.(check (list int)) "" expected result);
        ] );
      ( "IntPolynom.mul",
        [
          test "basic" (fun () ->
              let p1 = IntPolynom.of_list [ 1; 2 ] in
              let p2 = IntPolynom.of_list [ 3; 4; 5 ] in
              let result = IntPolynom.mul p1 p2 |> IntPolynom.coefficients in
              let expected = [ 3; 10; 13; 10 ] in
              Alcotest.(check (list int)) "" expected result);
          test "with negative" (fun () ->
              let p1 = IntPolynom.of_list [ 1; 2; -3; 4; -5 ] in
              let p2 = IntPolynom.of_list [ -7; -1; 7; -3; -5 ] in
              let result = IntPolynom.mul p1 p2 |> IntPolynom.coefficients in
              let expected = [ -7; -15; 26; -14; -1; 32; -32; -5; 25 ] in
              Alcotest.(check (list int)) "" expected result);
          test "with zeros" (fun () ->
              let p1 = IntPolynom.of_list [ 1; 0; -3; 0; -5 ] in
              let p2 = IntPolynom.of_list [ 0; -1; 7; 0; -5 ] in
              let result = IntPolynom.mul p1 p2 |> IntPolynom.coefficients in
              let expected = [ 0; -1; 7; 3; -26; 5; -20; 0; 25 ] in
              Alcotest.(check (list int)) "" expected result);
          test "zero first" (fun () ->
              let p1 = IntPolynom.of_list [ 0 ] in
              let p2 = IntPolynom.of_list [ 1; 0; -3; 0; -5 ] in
              let result = IntPolynom.mul p1 p2 |> IntPolynom.coefficients in
              let expected = [ 0 ] in
              Alcotest.(check (list int)) "" expected result);
          test "zero second" (fun () ->
              let p1 = IntPolynom.of_list [ 1; 0; -3; 0; -5 ] in
              let p2 = IntPolynom.of_list [ 0 ] in
              let result = IntPolynom.mul p1 p2 |> IntPolynom.coefficients in
              let expected = [ 0 ] in
              Alcotest.(check (list int)) "" expected result);
          test "commutative" (fun () ->
              let p1 = IntPolynom.of_list [ 1; 0; -3; 0; -5; 0; -1; 342; 1 ] in
              let p2 = IntPolynom.of_list [ 0; -1; 7; 0; -5; 45 ] in
              let a = IntPolynom.mul p1 p2 |> IntPolynom.coefficients in
              let b = IntPolynom.mul p2 p1 |> IntPolynom.coefficients in
              Alcotest.(check (list int)) "" a b);
        ] );
      ( "IntPolynom.derivate",
        [
          test "basic" (fun () ->
              let p = IntPolynom.of_list [ 5; 2; 3 ] in
              let result = IntPolynom.derivate p |> IntPolynom.coefficients in
              let expected = [ 2; 6 ] in
              Alcotest.(check (list int)) "" expected result);
          test "complex" (fun () ->
              let p = IntPolynom.of_list [ -9; 5; 0; 0; -1; -4; 5; 2; 3; -6 ] in
              let result = IntPolynom.derivate p |> IntPolynom.coefficients in
              let expected = [ 5; 0; 0; -4; -20; 30; 14; 24; -54 ] in
              Alcotest.(check (list int)) "" expected result);
          test "single" (fun () ->
              let p = IntPolynom.of_list [ 5 ] in
              let result = IntPolynom.derivate p |> IntPolynom.coefficients in
              let expected = [ 0 ] in
              Alcotest.(check (list int)) "" expected result);
          test "zero" (fun () ->
              let p = IntPolynom.of_list [ 0 ] in
              let result = IntPolynom.derivate p |> IntPolynom.coefficients in
              let expected = [ 0 ] in
              Alcotest.(check (list int)) "" expected result);
        ] );
      ( "IntPolynom.integrate",
        [
          test "basic" (fun () ->
              let p = IntPolynom.of_list [ 2; 6 ] in
              let result = IntPolynom.integrate p |> IntPolynom.coefficients in
              let expected = [ 0; 2; 3 ] in
              Alcotest.(check (list int)) "" expected result);
          test "complex" (fun () ->
              let p =
                IntPolynom.of_list [ 5; 0; 0; -4; -20; 30; 14; 24; -54 ]
              in
              let result = IntPolynom.integrate p |> IntPolynom.coefficients in
              let expected = [ 0; 5; 0; 0; -1; -4; 5; 2; 3; -6 ] in
              Alcotest.(check (list int)) "" expected result);
          test "zero" (fun () ->
              let p = IntPolynom.of_list [ 0 ] in
              let result = IntPolynom.integrate p |> IntPolynom.coefficients in
              let expected = [ 0 ] in
              Alcotest.(check (list int)) "" expected result);
        ] );
      ( "IntPolynom.quad",
        [
          test "basic" (fun () ->
              let p = IntPolynom.of_list [ 2; 6 ] in
              let result = IntPolynom.quad p 0 10 in
              let expected = 320 in
              Alcotest.(check int) "" expected result);
          test "floor" (fun () ->
              let p = IntPolynom.of_list [ 0; 4; 0; -4; 8 ] in
              let result = IntPolynom.quad p ~-5 5 in
              let expected = 6_250 in
              Alcotest.(check int) "" expected result);
          test "zero" (fun () ->
              let p = IntPolynom.of_list [ 0 ] in
              let result = IntPolynom.quad p ~-17 87 in
              let expected = 0 in
              Alcotest.(check int) "" expected result);
        ] );
    ]
