(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   ackermann.ml                                       :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: agrumbac <agrumbac@student.42.fr>          +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2018/05/28 14:39:49 by agrumbac          #+#    #+#             *)
(*   Updated: 2018/05/28 15:20:58 by agrumbac         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let rec ackermann m n =
	if m == 0 && n >= 0 then
		n + 1
	else if m > 0 && n == 0 then
		ackermann (m - 1) 1
	else if m > 0 && n > 0 then
		ackermann (m - 1) (ackermann m (n - 1))
	else
		-1

(* ********************************** test ********************************** *)

let () =
	print_int (ackermann (-1) 7);
	print_char '\n';
	print_int (ackermann 42 (-1));
	print_char '\n';
	print_int (ackermann (-1) (-1));
	print_char '\n';
	print_int (ackermann 0 0);
	print_char '\n';
	print_int (ackermann 2 3);
	print_char '\n';
	print_int (ackermann 4 1);
	print_char '\n';
	print_int (ackermann 1 0);
	print_char '\n';
	print_int (ackermann 2 0);
	print_char '\n';
	print_int (ackermann 3 0);
	print_char '\n';
	print_int (ackermann 2 5);
	print_char '\n'
