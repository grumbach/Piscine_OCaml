(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   fuel.ml                                            :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: agrumbac <agrumbac@student.42.fr>          +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2018/06/07 16:44:04 by agrumbac          #+#    #+#             *)
(*   Updated: 2018/06/07 16:47:50 by agrumbac         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

class methane =
	object (self)
		inherit Alkane.alkane 1
	end

class ethane =
	object (self)
		inherit Alkane.alkane 2
	end

class propane =
	object (self)
		inherit Alkane.alkane 3
	end

class butane =
	object (self)
		inherit Alkane.alkane 4
	end

class pentane =
	object (self)
		inherit Alkane.alkane 5
	end

class hexane =
	object (self)
		inherit Alkane.alkane 6
	end

class heptane =
	object (self)
		inherit Alkane.alkane 7
	end

class octane =
	object (self)
		inherit Alkane.alkane 8
	end

class nonane =
	object (self)
		inherit Alkane.alkane 9
	end
