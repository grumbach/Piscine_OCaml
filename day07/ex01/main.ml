(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   main.ml                                            :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: agrumbac <agrumbac@student.42.fr>          +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2018/06/06 16:44:40 by agrumbac          #+#    #+#             *)
(*   Updated: 2018/06/06 17:30:54 by agrumbac         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let () =
	let doc = new Doctor.doctor ("doc") (42) in
	print_endline doc#to_string;
	doc#talk;
	doc#travel_in_time 42 45;
	print_endline doc#to_string;
	doc#use_sonic_screwdriver;
	(* doc#regenerate; *)
	print_endline doc#to_string
