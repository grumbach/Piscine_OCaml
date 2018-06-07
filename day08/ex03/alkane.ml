(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   alkane.ml                                          :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: agrumbac <agrumbac@student.42.fr>          +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2018/06/07 16:12:04 by agrumbac          #+#    #+#             *)
(*   Updated: 2018/06/07 17:08:38 by agrumbac         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

class alkane n =
	object (self)
		method formula:string = self#compute_formula n
		method name:string = self#compute_name n

		method private compute_name (n:int) =
			match n with
			| 1 ->  "methane"
			| 2 ->  "ethane"
			| 3 ->  "propane"
			| 4 ->  "butane"
			| 5 ->  "pentane"
			| 6 ->  "hexane"
			| 7 ->  "heptane"
			| 8 ->  "octane"
			| 9 ->  "nonane"
			| 10 -> "decane"
			| 11 -> "hendecane"
			| 12 -> "dodecane"
			| _ ->  "wtfane"

		method private compute_formula (n:int) =
		if n > 0 && n <= 12 then
			"Alkane: C" ^ (if n > 1 then (string_of_int n) else "") ^ "H" ^ (string_of_int (2 * n + 2))
		else "invalid alkane"

		method private generate_atoms (n:int) :Atom.atom list =
			match n with
			| 0 -> [new Mendeleev.hydrogen;new Mendeleev.hydrogen]
			| _ -> [new Mendeleev.hydrogen;new Mendeleev.hydrogen;
				new Mendeleev.carbon] @ self#generate_atoms (n - 1)

		method to_string = "Alkane: " ^ self#name ^ " [" ^ self#formula ^ "]"

		method equals (alt:alkane) = (self#name = alt#name) && (self#formula = alt#formula)

	end
