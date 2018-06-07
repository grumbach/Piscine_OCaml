(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   molecule.ml                                        :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: agrumbac <agrumbac@student.42.fr>          +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2018/06/07 13:35:23 by agrumbac          #+#    #+#             *)
(*   Updated: 2018/06/07 15:33:53 by agrumbac         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

class virtual molecule new_name atom_list =
	object (self)
		val _atoms:Atom.atom list = atom_list

		method name:string = new_name
		method formula:string = self#compute_formula atom_list

		method private formula_of_atom_list (atoms:Atom.atom list) a n =
			match atoms with
			| [] -> a#symbol ^ (if n > 1 then (string_of_int n) else "")
			| hd :: tl ->
			if a#equals hd then
				self#formula_of_atom_list tl a (n + 1)
			else
				a#symbol ^ (if n > 1 then (string_of_int n) else "") ^ self#formula_of_atom_list tl hd 1

		method private sort_atom_list (atoms:Atom.atom list) =
			let compare_atoms (atom1:Atom.atom) (atom2:Atom.atom) =
				String.compare (atom1#symbol) (atom2#symbol)
			in List.sort compare_atoms atoms


		method private compute_formula (atoms:Atom.atom list) =
			let sorted_list = self#sort_atom_list atoms in
			match sorted_list with
			| [] -> ""
			| hd :: tl -> self#formula_of_atom_list tl hd 1

		method to_string = "Molecule: " ^ self#name ^ " [" ^ self#formula ^ "]"

		method equals (alt:molecule) = (self#name = alt#name) && (self#formula = alt#formula)

	end
