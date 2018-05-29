(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   repeat_string.ml                                   :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: agrumbac <agrumbac@student.42.fr>          +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2018/05/28 14:03:46 by agrumbac          #+#    #+#             *)
(*   Updated: 2018/05/28 14:40:05 by agrumbac         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let rec repeat_string ?(str="x") n =
	if n < 0 then
		"Error"
	else if n = 0 then
		""
	else
		str ^ repeat_string ~str:str (n - 1)

(* ********************************** test ********************************** *)

let () =
	print_endline (repeat_string (-42));
	print_endline (repeat_string (-1));
	print_endline (repeat_string 0);
	print_endline (repeat_string 1);
	print_endline (repeat_string 5);
	print_endline (repeat_string 42);
	print_endline "-------------";
	print_endline (repeat_string ~str:"hey" (-42));
	print_endline (repeat_string ~str:"hey" (-1));
	print_endline (repeat_string ~str:"dont disp me" 0);
	print_endline (repeat_string ~str:"yo" 1);
	print_endline (repeat_string ~str:"a" 5);
	print_endline (repeat_string ~str:"hey" 42)
