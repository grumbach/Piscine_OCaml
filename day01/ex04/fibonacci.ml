(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   fibonacci.ml                                       :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: agrumbac <agrumbac@student.42.fr>          +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2018/05/28 16:00:10 by agrumbac          #+#    #+#             *)
(*   Updated: 2018/05/28 17:06:33 by agrumbac         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let fibonacci nb =
	if nb < 0 then (-1)
	else let rec fib a b n =
		if n <= 0 then
			a
		else
			fib b (a + b) (n - 1)
	in fib 0 1 nb

(* ********************************** test ********************************** *)

let () =
	print_int (fibonacci (-42));
	print_char '\n';
	print_int (fibonacci (-1));
	print_char '\n';
	print_int (fibonacci 0);
	print_char '\n';
	print_int (fibonacci 1);
	print_char '\n';
	print_int (fibonacci 3);
	print_char '\n';
	print_int (fibonacci 6);
	print_char '\n';
	print_int (fibonacci 42);
	print_char '\n';
	print_int (fibonacci 234321234); (* stackoverflow test *)
	print_char '\n'
