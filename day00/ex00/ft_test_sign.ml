(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   ft_test_sign.ml                                    :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: agrumbac <agrumbac@student.42.fr>          +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2018/05/26 22:25:59 by agrumbac          #+#    #+#             *)
(*   Updated: 2018/05/27 17:40:57 by agrumbac         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let ft_test_sign a =
	if a >= 0 then
		print_endline "positive"
	else
		print_endline "negative"

(* ********************************** test ********************************** *)

let main () =
	ft_test_sign 42;
	ft_test_sign 0;
	ft_test_sign 1073741824;
	ft_test_sign (-1);
	ft_test_sign (-42);
	ft_test_sign (-1073741824)

let () = main ()
