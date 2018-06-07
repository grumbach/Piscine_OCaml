(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   main.ml                                            :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: agrumbac <agrumbac@student.42.fr>          +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2018/06/07 13:21:25 by agrumbac          #+#    #+#             *)
(*   Updated: 2018/06/07 17:06:17 by agrumbac         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let () =
	let methane = new Alkane.alkane 1
	and methane2 = new Alkane.alkane 1
	and ethane = new Alkane.alkane 2
	and propane = new Alkane.alkane 3
	and butane = new Alkane.alkane 4
	and pentane = new Alkane.alkane 5
	and hexane = new Alkane.alkane 6
	and heptane = new Alkane.alkane 7
	and octane = new Alkane.alkane 8
	and nonane = new Alkane.alkane 9
	and decane = new Alkane.alkane 10
	and hendecane = new Alkane.alkane 11
	and dodecane = new Alkane.alkane 12
	and wtfane0 = new Alkane.alkane 0
	and wtfane12 = new Alkane.alkane 13
	in
	print_endline methane#to_string;
	print_endline ethane#to_string;
	print_endline propane#to_string;
	print_endline butane#to_string;
	print_endline pentane#to_string;
	print_endline hexane#to_string;
	print_endline heptane#to_string;
	print_endline octane#to_string;
	print_endline nonane#to_string;
	print_endline decane#to_string;
	print_endline hendecane#to_string;
	print_endline dodecane#to_string;
	print_endline wtfane0#to_string;
	print_endline wtfane12#to_string;
	if methane#equals ethane then
		print_endline "--> wtf???"
	else if methane#equals methane2 then
		print_endline "--> Equal method works!"
