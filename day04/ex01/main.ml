(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   main.ml                                            :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: agrumbac <agrumbac@student.42.fr>          +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2018/06/01 16:40:03 by agrumbac          #+#    #+#             *)
(*   Updated: 2018/06/01 18:03:14 by agrumbac         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let () =
	let rec string_of_value_list toStrfunc lst =
		match lst with
		| [] -> ""
		| hd :: tl -> (toStrfunc hd) ^ " " ^ (string_of_value_list toStrfunc tl)
	and string_of_int_value_list toIntfunc lst =
		match lst with
		| [] -> ""
		| hd :: tl -> (string_of_int (toIntfunc hd)) ^ " " ^ (string_of_int_value_list toIntfunc tl)
	and get_next toStrfunc lst =
		match lst with
		| hd :: tl -> (toStrfunc (Value.next hd))
		| _ -> ""
	and get_prev toStrfunc lst =
		match lst with
		| hd :: nxt :: tl -> (toStrfunc (Value.prev nxt))
		| _ -> ""
	in
	print_endline (string_of_value_list Value.toStringVerbose Value.all);
	print_endline (string_of_value_list Value.toString Value.all);
	print_endline (string_of_int_value_list Value.toInt Value.all);
	print_endline ("after  2  : " ^ (get_next Value.toString Value.all));
	print_endline ("before 3  : " ^ (get_prev Value.toString Value.all))
