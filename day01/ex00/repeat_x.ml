(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   repeat_x.ml                                        :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: agrumbac <agrumbac@student.42.fr>          +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2018/05/28 13:45:35 by agrumbac          #+#    #+#             *)
(*   Updated: 2018/05/28 13:58:08 by agrumbac         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let rec repeat_x n =
	if n < 0 then
		"Error"
	else if n == 0 then
		""
	else
		"x" ^ repeat_x (n - 1)

(* ********************************** test ********************************** *)

let () =
	print_endline (repeat_x (-42));
	print_endline (repeat_x (-1));
	print_endline (repeat_x 0);
	print_endline (repeat_x 1);
	print_endline (repeat_x 5);
	print_endline (repeat_x 42)
