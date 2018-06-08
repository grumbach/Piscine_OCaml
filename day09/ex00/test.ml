(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   test.ml                                            :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: agrumbac <agrumbac@student.42.fr>          +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2018/06/08 13:43:49 by agrumbac          #+#    #+#             *)
(*   Updated: 2018/06/08 14:00:29 by agrumbac         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let () =
	let eat:Clock.Watchtower.hour = 1
	and sleep:Clock.Watchtower.hour = 10
	and code:Clock.Watchtower.hour = 15
	in
	let wasted_time:Clock.Watchtower.hour = Clock.Watchtower.add eat sleep in
	let day:Clock.Watchtower.hour = Clock.Watchtower.sub code wasted_time in
	let tutu:Clock.Watchtower.hour = Clock.Watchtower.add wasted_time wasted_time in
	let tata:Clock.Watchtower.hour = Clock.Watchtower.sub sleep code in
	print_endline ("1 + 10  = " ^ (string_of_int wasted_time));
	print_endline ("15 - 11 = " ^ (string_of_int day));
	print_endline ("11 + 11 = " ^ (string_of_int tutu));
	print_endline ("10 - 15 = " ^ (string_of_int tata));
	print_endline ("zero: " ^ (string_of_int Clock.Watchtower.zero))
