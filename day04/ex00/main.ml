(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   main.ml                                            :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: agrumbac <agrumbac@student.42.fr>          +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2018/06/01 16:40:03 by agrumbac          #+#    #+#             *)
(*   Updated: 2018/06/01 16:53:05 by agrumbac         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let () =
	let rec string_of_color_list toStrfunc lst =
		match lst with
		| [] -> ""
		| hd :: tl -> (toStrfunc hd) ^ " " ^ (string_of_color_list toStrfunc tl)
	in
	print_endline (string_of_color_list Color.toStringVerbose Color.all);
	print_endline (string_of_color_list Color.toString Color.all)
