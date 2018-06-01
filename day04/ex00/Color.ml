(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   Color.ml                                           :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: agrumbac <agrumbac@student.42.fr>          +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2018/06/01 16:40:02 by agrumbac          #+#    #+#             *)
(*   Updated: 2018/06/01 16:45:07 by agrumbac         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

type t = Spade | Heart | Diamond | Club

let all = [Spade; Heart; Diamond; Club]

let toString col =
	match col with
	| Spade -> "S"
	| Heart -> "H"
	| Diamond -> "D"
	| Club -> "C"

let toStringVerbose col =
	match col with
	| Spade -> "Spade"
	| Heart -> "Heart"
	| Diamond -> "Diamond"
	| Club -> "Club"
