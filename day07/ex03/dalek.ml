(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   dalek.ml                                           :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: agrumbac <agrumbac@student.42.fr>          +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2018/06/06 17:31:48 by agrumbac          #+#    #+#             *)
(*   Updated: 2018/06/06 18:09:51 by agrumbac         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

class dalek =
	object
		initializer Random.self_init ()

		val _name:string = "Dalek" ^
			begin
				let rand = Random.int 4 in
				match rand with
				| 0 -> "tic"
				| 1 -> "tac"
				| 2 -> "toc"
				| _ -> "toe"
			end
		val _hp:int = 100
		val mutable _shield:bool = true

		method to_string = _name ^ " has " ^ (string_of_int _hp) ^ "hp, and has " ^ (if _shield then "activated its shield!" else "no shield!")

		method talk =
			let rand = Random.int 4 in
			print_endline (
			match rand with
			| 0 -> "Explain! Explain!"
			| 1 -> "Exterminate! Exterminate!"
			| 2 -> "I obey!"
			| _ -> "You are the Doctor! You are the enemy of the Daleks!")

		method exterminate (someone:People.people) =
			_shield <- not _shield;
			someone#die

		method die = print_endline "Emergency Temporal Shift!"
	end
