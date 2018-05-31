(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   cipher.ml                                          :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: agrumbac <agrumbac@student.42.fr>          +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2018/05/31 21:27:33 by agrumbac          #+#    #+#             *)
(*   Updated: 2018/06/01 15:13:03 by agrumbac         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let caesar nbr str =
	let rot c =
		let int_c = int_of_char c in
		let rot_from start_char =
			char_of_int (((((int_c - start_char + nbr) mod 26) + 26) mod 26) + start_char)
		in match c with
		| 'A'..'Z' -> rot_from (int_of_char 'A')
		| 'a'..'z' -> rot_from (int_of_char 'a')
		| _ -> c
	in String.map rot str

let rot42 str =
	caesar 42 str

let xor nbr str =
	let xorchar c = char_of_int((int_of_char c) lxor nbr)
	in String.map xorchar str

let rec ft_crypt str funlist =
	match funlist with
	| head::tail -> ft_crypt (head str) tail
	| [] -> str
