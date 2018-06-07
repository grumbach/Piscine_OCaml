(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   mendeleev.ml                                       :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: agrumbac <agrumbac@student.42.fr>          +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2018/06/07 13:07:36 by agrumbac          #+#    #+#             *)
(*   Updated: 2018/06/07 13:23:39 by agrumbac         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

class carbon =
	object (self)
		inherit Atom.atom "carbon" "C" 6
	end

class hydrogen =
	object (self)
		inherit Atom.atom "hydrogen" "H" 1
	end

class oxygen =
	object (self)
		inherit Atom.atom "oxygen" "O" 8
	end

class helium =
	object (self)
		inherit Atom.atom "helium" "H" 2
	end

class potassium =
	object (self)
		inherit Atom.atom "potassium" "K" 19
	end

class molybdenum =
	object (self)
		inherit Atom.atom "molybdenum" "Mo" 42
	end

class scandium =
	object (self)
		inherit Atom.atom "scandium" "Sc" 21
	end

class yttrium =
	object (self)
		inherit Atom.atom "yttrium" "Y" 39
	end

class chromium =
	object (self)
		inherit Atom.atom "chromium" "Cr" 24
	end

class ethereum =
	object (self)
		inherit Atom.atom "ethereum" "Eth" 126
	end
