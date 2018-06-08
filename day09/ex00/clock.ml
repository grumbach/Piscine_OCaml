(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   clock.ml                                           :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: agrumbac <agrumbac@student.42.fr>          +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2018/06/08 13:33:13 by agrumbac          #+#    #+#             *)
(*   Updated: 2018/06/08 13:58:40 by agrumbac         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

module Watchtower =
struct
	type hour = int

	let zero :hour = 0

	let add (h:hour) (x:hour) :hour = (((h + x) mod 12) + 12 ) mod 12
	let sub (h:hour) (x:hour) :hour = (((h - x) mod 12) + 12 ) mod 12
end
