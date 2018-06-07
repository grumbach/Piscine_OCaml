(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   main.ml                                            :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: agrumbac <agrumbac@student.42.fr>          +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2018/06/07 13:21:25 by agrumbac          #+#    #+#             *)
(*   Updated: 2018/06/07 13:29:20 by agrumbac         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let () =
	let c = new Mendeleev.carbon
	and h = new Mendeleev.hydrogen
	and o = new Mendeleev.oxygen
	and he = new Mendeleev.helium
	and k = new Mendeleev.potassium
	and mo = new Mendeleev.molybdenum
	and sc = new Mendeleev.scandium
	and y = new Mendeleev.yttrium
	and cr = new Mendeleev.chromium
	and eth = new Mendeleev.ethereum
	and eth2 = new Mendeleev.ethereum
	in
	print_endline c#to_string;
	print_endline h#to_string;
	print_endline o#to_string;
	print_endline he#to_string;
	print_endline k#to_string;
	print_endline mo#to_string;
	print_endline sc#to_string;
	print_endline y#to_string;
	print_endline cr#to_string;
	print_endline eth#to_string;
	if eth#equals cr then
		print_endline "--> Web 3.0 realeased! (wtf??)"
	else if eth#equals eth2 then
		print_endline "--> Equal method works!"
