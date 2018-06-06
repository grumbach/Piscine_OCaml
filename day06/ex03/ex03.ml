(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   ex03.ml                                            :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: agrumbac <agrumbac@student.42.fr>          +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2018/06/06 14:11:45 by agrumbac          #+#    #+#             *)
(*   Updated: 2018/06/06 14:34:00 by agrumbac         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

module type FIXED = sig
	type t
	val of_float : float -> t
	val of_int : int -> t
	val to_float : t -> float
	val to_int : t -> int
	val to_string : t -> string
	val zero : t
	val one : t
	val succ : t -> t
	val pred : t -> t
	val min : t -> t -> t
	val max : t -> t -> t
	val gth : t -> t -> bool
	val lth : t -> t -> bool
	val gte : t -> t -> bool
	val lte : t -> t -> bool
	val eqp : t -> t -> bool (** physical equality *)
	val eqs : t -> t -> bool (** structural equality *)
	val add : t -> t -> t
	val sub : t -> t -> t
	val mul : t -> t -> t
	val div : t -> t -> t
	val foreach : t -> t -> (t -> unit) -> unit
end

module type FRACTIONNAL_BITS = sig val bits : int end

module type MAKE = functor (FractionnalBits: FRACTIONNAL_BITS) -> FIXED

module Make : MAKE =
	functor (FractionnalBits: FRACTIONNAL_BITS) ->
	struct
		type t = int
		let point = (FractionnalBits.bits)
		let zero = 0
		let one = (1 lsl point)
		let of_float flo = int_of_float(ceil(flo *. float_of_int(one)))
		let of_int integ = (integ lsl point)
		let to_float frac = (float_of_int(frac) /. float_of_int(one))
		let to_int frac = (frac lsr point)
		let to_string frac = string_of_float(to_float(frac))
		let succ frac = frac + one
		let pred frac = frac - one
		let min frac1 frac2 = if frac1 > frac2 then frac2 else frac1
		let max frac1 frac2 = if frac1 < frac2 then frac2 else frac1
		let gth frac1 frac2 = if frac1 > frac2 then true else false
		let lth frac1 frac2 = if frac1 < frac2 then true else false
		let gte frac1 frac2 = if frac1 >= frac2 then true else false
		let lte frac1 frac2 = if frac1 <= frac2 then true else false
		let eqp frac1 frac2 = if frac1 == frac2 then true else false
		let eqs frac1 frac2 = if frac1 = frac2 then true else false
		let add frac1 frac2 = frac1 + frac2
		let sub frac1 frac2 = frac1 - frac2
		let mul frac1 frac2 = frac1 * frac2
		let div frac1 frac2 = if frac2 <> 0 then frac1 / frac2 else 0
		let foreach frac1 frac2 funct =
			let rec for_next elem =
				if elem > frac2 then ()
				else (funct (elem); for_next (elem + 1))
		in for_next frac1
	end

(* test *)

module Fixed4 : FIXED = Make (struct let bits = 4 end)
module Fixed8 : FIXED = Make (struct let bits = 8 end)

let () =
	let x8 = Fixed8.of_float 21.10 in
	let y8 = Fixed8.of_float 21.32 in
	let r8 = Fixed8.add x8 y8 in
	print_endline (Fixed8.to_string r8);
	Fixed4.foreach (Fixed4.zero) (Fixed4.one) (fun f -> print_endline (Fixed4.to_string f))
