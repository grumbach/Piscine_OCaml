(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   uncipher.ml                                        :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: agrumbac <agrumbac@student.42.fr>          +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2018/05/31 21:27:32 by agrumbac          #+#    #+#             *)
(*   Updated: 2018/06/01 12:39:22 by agrumbac         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let uncaesar nbr str =
	Cipher.caesar (nbr * (-1)) str

let unrot42 str =
	uncaesar 42 str

let xor nbr str =
	Cipher.xor nbr str

let rec ft_uncrypt str funlist =
	match funlist with
	| head::tail -> ft_uncrypt (head str) tail
	| [] -> str
