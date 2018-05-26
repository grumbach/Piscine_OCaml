(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   ft_string_all.ml                                   :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: agrumbac <agrumbac@student.42.fr>          +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2018/05/27 19:54:56 by agrumbac          #+#    #+#             *)
(*   Updated: 2018/05/28 00:08:46 by agrumbac         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let rec ft_string_apply predicate s n =
	if n < 0 then
		true
	else
		predicate (String.get s n) && ft_string_apply predicate s (n - 1)

let ft_string_all predicate s =
	ft_string_apply predicate s (String.length s - 1)

(* ********************************** test ********************************** *)

let is_digit c = c >= '0' && c <= '9'

let test_for s =
	print_string s;
	if ft_string_all is_digit s then
		print_string " is all digits\n"
	else
		print_string " is NOT all digits\n"

let main () =
	test_for "0";
	test_for "42";
	test_for "23465543";
	test_for "23465531254s43";
	test_for "2346553125443s";
	test_for "l2346553125443";
	test_for "lol"

let () = main ()
