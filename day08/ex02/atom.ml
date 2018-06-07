(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   atom.ml                                            :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: agrumbac <agrumbac@student.42.fr>          +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2018/06/07 12:10:44 by agrumbac          #+#    #+#             *)
(*   Updated: 2018/06/07 13:27:52 by agrumbac         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

class virtual atom new_name new_symbol new_atomic_number =
	object (self)
		method name:string = new_name
		method symbol:string = new_symbol
		method atomic_number:int = new_atomic_number

		method to_string = "Atom: " ^ self#name ^ " [" ^ self#symbol ^ "] (" ^ (string_of_int (self#atomic_number)) ^ ")"

		method equals (alt:atom) = (self#name = alt#name) && (self#symbol = alt#symbol) && (self#atomic_number = alt#atomic_number)

	end
