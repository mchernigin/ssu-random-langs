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

module Make (Coefficient : CoefficientType) :
  Polynom with type t = Coefficient.t
