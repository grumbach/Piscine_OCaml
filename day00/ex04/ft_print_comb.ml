(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   ft_print_comb.ml                                   :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: agrumbac <agrumbac@student.42.fr>          +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2018/05/27 18:05:29 by agrumbac          #+#    #+#             *)
(*   Updated: 2018/05/27 18:24:37 by agrumbac         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let rec ft_print_comb_from a b c =
	print_int a;
	print_int b;
	print_int c;
	if a < 7 then
		print_string ", ";
	if c < 9 then
		ft_print_comb_from a b (c + 1)
	else if b < 8 then
		ft_print_comb_from a (b + 1) (b + 2)
	else if a < 7 then
		ft_print_comb_from (a + 1) (a + 2) (a + 3)

let ft_print_comb () =
	ft_print_comb_from 0 1 2;
	print_string "\n"

(* ********************************** test ********************************** *)

let main () =
	ft_print_comb ()

let () = main ()
