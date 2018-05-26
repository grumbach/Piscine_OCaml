(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   ft_power.ml                                        :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: agrumbac <agrumbac@student.42.fr>          +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2018/05/27 17:14:24 by agrumbac          #+#    #+#             *)
(*   Updated: 2018/05/27 17:40:51 by agrumbac         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let rec ft_power n e =
	if e <= 0 then
		1
	else
		n * ft_power n (e - 1)

(* ********************************** test ********************************** *)

let print_int_endl n =
	print_int n;
	print_char '\n'

let main () =
	print_int_endl (ft_power 2 4) ;
	print_int_endl (ft_power (-3) 0) ;
	print_int_endl (ft_power (-3) 5) ;
	print_int_endl (ft_power 0 5) ;
	print_int_endl (ft_power (-2) 42) ;
	print_int_endl (ft_power 0 (-1)) ;
	print_int_endl (ft_power 42 (-42)) ;
	print_int_endl (ft_power 0 0) ;
	print_int_endl 42

let () = main ()
