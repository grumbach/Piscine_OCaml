(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   reaction.ml                                        :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: agrumbac <agrumbac@student.42.fr>          +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2018/06/07 17:40:46 by agrumbac          #+#    #+#             *)
(*   Updated: 2018/06/07 18:02:41 by agrumbac         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

class virtual reaction (reactants: (Molecule.molecule * int) list)
						(products: (Molecule.molecule * int) list) =
	object (self)

	method virtual get_start : (Molecule.molecule * int) list
	method virtual get_result : (Molecule.molecule * int) list

	method virtual balance : reaction

	method virtual is_balanced : bool

	end
