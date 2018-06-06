(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   army.ml                                            :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: agrumbac <agrumbac@student.42.fr>          +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2018/06/06 18:10:43 by agrumbac          #+#    #+#             *)
(*   Updated: 2018/06/06 18:21:33 by agrumbac         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

class ['a] army (army_list:'a list) =
	object
		val _attribute:'a list = army_list

		method add (new_soldier:'a) =
		{<
			_attribute = _attribute @ [new_soldier]
		>}

		method delete =
		{<
			_attribute =
			begin
				match _attribute with
				| [] -> []
				| hd::tl -> tl
			end
		>}

	end
