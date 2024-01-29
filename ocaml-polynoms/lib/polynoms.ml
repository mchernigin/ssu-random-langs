module type CoefficientType = sig
  type t

  val compare : t -> t -> int
  val negate : t -> t
  val add : t -> t -> t
  val mul : t -> t -> t
  val div : t -> t -> t
  val abs : t -> t
  val to_string : t -> string
  val of_float : float -> t
end

module type Polynom = sig
  type t
  type polynom

  val of_list : t list -> polynom
  val coefficients : polynom -> t list
  val to_string : polynom -> string
  val value : polynom -> t -> t
  val negate : polynom -> polynom
  val add : polynom -> polynom -> polynom
  val sub : polynom -> polynom -> polynom
  val mul : polynom -> polynom -> polynom
  val derivate : polynom -> polynom
  val integrate : polynom -> polynom
  val quad : polynom -> t -> t -> t
end

module Make (Coefficient : CoefficientType) = struct
  type t = Coefficient.t
  type polynom = Poly of t list

  let cmp c f = Coefficient.compare c (Coefficient.of_float f)

  let normalize (Poly l) =
    let rec removeLeadingZeros = function
      | [] -> []
      | x :: xs as l -> if cmp x 0. = 0 then removeLeadingZeros xs else l
    in
    let unempty = function [] -> [ Coefficient.of_float 0. ] | l -> l in
    Poly (l |> List.rev |> removeLeadingZeros |> List.rev |> unempty)

  let of_list l = Poly l |> normalize
  let coefficients (Poly l) = l

  let to_string (Poly l) =
    let rec to_string_inner l p =
      let len = List.length l in
      let k = List.hd l in
      let sign =
        if cmp k 0. < 0 then "- "
        else if cmp k 0. > 0 && len > 1 then "+ "
        else ""
      in
      let space = if cmp k 0. <> 0 && len > 1 then " " else "" in
      let k_str =
        if (cmp k 0. = 0 || cmp k 1. = 0) && (p <> 0 || len > 1) then ""
        else k |> Coefficient.abs |> Coefficient.to_string
      in
      let x =
        if p = 0 || cmp k 0. = 0 then ""
        else if p = 1 then "x"
        else "x^" ^ string_of_int p
      in
      let rest = if len = 1 then "" else to_string_inner (List.tl l) (p + 1) in
      rest ^ space ^ sign ^ k_str ^ x
    in
    to_string_inner l 0

  let value (Poly l) x =
    let rec pow_inner = function
      | _, 0, res -> res
      | x, power, res -> pow_inner (x, power - 1, Coefficient.mul res x)
    in
    let pow x i = pow_inner (x, i, Coefficient.of_float 1.) in
    let sum = List.fold_left Coefficient.add (Coefficient.of_float 0.) in
    List.mapi (fun i c -> Coefficient.mul c (pow x i)) l |> sum

  let negate (Poly l) = Poly (List.map Coefficient.negate l)

  let rec add_lists = function
    | [], l -> l
    | l, [] -> l
    | x :: xs, y :: ys -> Coefficient.add x y :: add_lists (xs, ys)

  let add (Poly l1) (Poly l2) = Poly (add_lists (l1, l2)) |> normalize
  let sub p1 p2 = add p1 (negate p2)

  let mul (Poly l1) (Poly l2) =
    let scale l s = List.map (fun x -> Coefficient.mul x s) l in
    let rec mul_lists = function
      | [], _ -> []
      | x :: xs, l2 ->
          add_lists (scale l2 x, Coefficient.of_float 0. :: mul_lists (xs, l2))
    in
    Poly (mul_lists (l1, l2)) |> normalize

  let derivate (Poly l) =
    let derivate_one power x =
      Coefficient.mul x (power |> float_of_int |> Coefficient.of_float)
    in
    Poly (List.mapi derivate_one l |> List.tl) |> normalize

  let integrate (Poly l) =
    let integrate_one power x =
      Coefficient.div x (power + 1 |> float_of_int |> Coefficient.of_float)
    in
    Poly (Coefficient.of_float 0. :: List.mapi integrate_one l) |> normalize

  let quad p a b =
    let integral = integrate p in
    let value_in_a = value integral a in
    let value_in_b = value integral b in
    Coefficient.add value_in_b (Coefficient.negate value_in_a)
end
