module type CoefficientType =
  sig
    type t
    (* val compare : t -> t -> int *)
    (* val negate : t -> t *)
    (* val add : t -> t -> t *)
    (* val mul : t -> t -> t *)
    (* val divide : t -> t -> t *)
    (* val modulo : t -> t -> t *)
    (* val abs : t -> t *)
    (* val sign : t -> t *)
    val to_string : t -> string
    (* val of_float : float -> t *)
  end

module type Polynom =
  sig
    type t
    type polynom
    val of_list : t list -> polynom
    val to_string : polynom -> string
  end

module Make (Coefficient : CoefficientType) : Polynom with type t =
  Coefficient.t
