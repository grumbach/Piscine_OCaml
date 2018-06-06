(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   people.ml                                          :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: agrumbac <agrumbac@student.42.fr>          +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2018/06/06 15:26:33 by agrumbac          #+#    #+#             *)
(*   Updated: 2018/06/06 16:44:37 by agrumbac         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

class people (name:string) =
	object
		initializer print_endline "Someone was created!"

		val _name:string = name
		val _hp:int = 100

		method to_string = _name ^ " has " ^ (string_of_int _hp) ^ "hp"
		method talk = print_endline ("I’m [" ^ _name ^ "]! Do you know the Doctor?")
		method die = print_endline "Aaaarghh!"
	end
