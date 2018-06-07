(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   main.ml                                            :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: agrumbac <agrumbac@student.42.fr>          +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2018/06/07 13:21:25 by agrumbac          #+#    #+#             *)
(*   Updated: 2018/06/07 15:31:30 by agrumbac         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let () =
	let water = new Nature.water
	and carbondioxyde = new Nature.carbondioxyde
	and trinitrotoluene = new Nature.trinitrotoluene
	and cytosine = new Nature.cytosine
	and guanine = new Nature.guanine
	and adenine = new Nature.adenine
	and adenine2 = new Nature.adenine
	in
	print_endline water#to_string;
	print_endline carbondioxyde#to_string;
	print_endline trinitrotoluene#to_string;
	print_endline cytosine#to_string;
	print_endline guanine#to_string;
	print_endline adenine#to_string;
	if adenine#equals guanine then
		print_endline "--> wtf???"
	else if adenine#equals adenine2 then
		print_endline "--> Equal method works!"
