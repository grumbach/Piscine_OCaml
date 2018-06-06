(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   main.ml                                            :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: agrumbac <agrumbac@student.42.fr>          +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2018/06/06 16:44:40 by agrumbac          #+#    #+#             *)
(*   Updated: 2018/06/06 18:03:03 by agrumbac         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let () =
	let doc = new Doctor.doctor ("doc") (42)
	and dal = new Dalek.dalek
	and guy = new People.people ("kek")
	in
	print_endline doc#to_string;
	print_endline dal#to_string;
	print_endline guy#to_string;
	doc#talk;
	dal#talk;
	guy#talk;
	dal#exterminate guy;
	doc#use_sonic_screwdriver;
	dal#die
