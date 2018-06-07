(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   nature.ml                                          :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: agrumbac <agrumbac@student.42.fr>          +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2018/06/07 14:33:03 by agrumbac          #+#    #+#             *)
(*   Updated: 2018/06/07 15:29:59 by agrumbac         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

class water =
	object (self)
		inherit Molecule.molecule "water" [new Mendeleev.hydrogen;
			new Mendeleev.hydrogen; new Mendeleev.oxygen]
	end

class carbondioxyde =
	object (self)
		inherit Molecule.molecule "carbondioxyde" [new Mendeleev.carbon;
			new Mendeleev.oxygen; new Mendeleev.oxygen]
	end

class trinitrotoluene =
	object (self)
		inherit Molecule.molecule "trinitrotoluene" [new Mendeleev.nitrogen;
			new Mendeleev.nitrogen; new Mendeleev.nitrogen;
			new Mendeleev.hydrogen; new Mendeleev.hydrogen;
			new Mendeleev.hydrogen;new Mendeleev.hydrogen;
			new Mendeleev.hydrogen;
			new Mendeleev.oxygen; new Mendeleev.oxygen;
			new Mendeleev.oxygen; new Mendeleev.oxygen;
			new Mendeleev.oxygen; new Mendeleev.oxygen;
			new Mendeleev.carbon; new Mendeleev.carbon;
			new Mendeleev.carbon; new Mendeleev.carbon;
			new Mendeleev.carbon; new Mendeleev.carbon;
			new Mendeleev.carbon;]
	end

class cytosine =
	object (self)
		inherit Molecule.molecule "cytosine"
		[new Mendeleev.nitrogen;new Mendeleev.nitrogen;new Mendeleev.nitrogen;
		new Mendeleev.nitrogen;new Mendeleev.nitrogen;new Mendeleev.nitrogen;
		new Mendeleev.nitrogen;new Mendeleev.nitrogen;new Mendeleev.nitrogen;
		new Mendeleev.nitrogen;new Mendeleev.nitrogen;new Mendeleev.nitrogen;
		new Mendeleev.nitrogen;new Mendeleev.nitrogen;new Mendeleev.nitrogen;
		new Mendeleev.nitrogen;new Mendeleev.nitrogen;new Mendeleev.nitrogen;
		new Mendeleev.nitrogen;new Mendeleev.nitrogen;new Mendeleev.nitrogen;
		new Mendeleev.nitrogen;new Mendeleev.nitrogen;new Mendeleev.nitrogen;
		new Mendeleev.nitrogen;new Mendeleev.nitrogen;new Mendeleev.nitrogen;
		new Mendeleev.nitrogen;new Mendeleev.nitrogen;new Mendeleev.nitrogen;
		new Mendeleev.hydrogen;new Mendeleev.hydrogen;new Mendeleev.hydrogen;
		new Mendeleev.hydrogen;new Mendeleev.hydrogen;
		new Mendeleev.carbon;new Mendeleev.carbon;new Mendeleev.carbon;
		new Mendeleev.carbon]
	end

class guanine =
	object (self)
		inherit Molecule.molecule "guanine"
		[new Mendeleev.nitrogen;new Mendeleev.nitrogen;new Mendeleev.nitrogen;
		new Mendeleev.nitrogen;new Mendeleev.nitrogen;new Mendeleev.nitrogen;
		new Mendeleev.nitrogen;new Mendeleev.nitrogen;new Mendeleev.nitrogen;
		new Mendeleev.nitrogen;new Mendeleev.nitrogen;new Mendeleev.nitrogen;
		new Mendeleev.nitrogen;new Mendeleev.nitrogen;new Mendeleev.nitrogen;
		new Mendeleev.nitrogen;new Mendeleev.nitrogen;new Mendeleev.nitrogen;
		new Mendeleev.nitrogen;new Mendeleev.nitrogen;new Mendeleev.nitrogen;
		new Mendeleev.nitrogen;new Mendeleev.nitrogen;new Mendeleev.nitrogen;
		new Mendeleev.nitrogen;new Mendeleev.nitrogen;new Mendeleev.nitrogen;
		new Mendeleev.nitrogen;new Mendeleev.nitrogen;new Mendeleev.nitrogen;
		new Mendeleev.nitrogen;new Mendeleev.nitrogen;new Mendeleev.nitrogen;
		new Mendeleev.nitrogen;new Mendeleev.nitrogen;new Mendeleev.nitrogen;
		new Mendeleev.nitrogen;new Mendeleev.nitrogen;new Mendeleev.nitrogen;
		new Mendeleev.nitrogen;new Mendeleev.nitrogen;new Mendeleev.nitrogen;
		new Mendeleev.nitrogen;new Mendeleev.nitrogen;new Mendeleev.nitrogen;
		new Mendeleev.nitrogen;new Mendeleev.nitrogen;new Mendeleev.nitrogen;
		new Mendeleev.nitrogen;new Mendeleev.nitrogen;
		new Mendeleev.carbon;new Mendeleev.carbon;new Mendeleev.carbon;
		new Mendeleev.carbon;new Mendeleev.carbon;
		new Mendeleev.hydrogen;new Mendeleev.hydrogen;new Mendeleev.hydrogen;
		new Mendeleev.hydrogen;new Mendeleev.hydrogen]
	end

class adenine =
	object (self)
		inherit Molecule.molecule "adenine"
		[new Mendeleev.carbon;new Mendeleev.carbon;new Mendeleev.carbon;
		new Mendeleev.carbon;new Mendeleev.carbon;
		new Mendeleev.nitrogen;new Mendeleev.nitrogen;new Mendeleev.nitrogen;
		new Mendeleev.nitrogen;new Mendeleev.nitrogen;
		new Mendeleev.hydrogen;new Mendeleev.hydrogen;new Mendeleev.hydrogen;
		new Mendeleev.hydrogen;new Mendeleev.hydrogen]
	end
