(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   sum.ml                                             :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: agrumbac <agrumbac@student.42.fr>          +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2018/06/05 13:33:50 by agrumbac          #+#    #+#             *)
(*   Updated: 2018/06/05 13:35:50 by agrumbac         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let sum a b = a +. b


(* ********************************** test ********************************** *)

let () =
	print_float (sum 42.0 0.42);
	print_endline "";
	print_float (sum 42.42 0.0);
	print_endline ""