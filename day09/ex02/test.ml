(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   test.ml                                            :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: agrumbac <agrumbac@student.42.fr>          +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2018/06/08 14:20:39 by agrumbac          #+#    #+#             *)
(*   Updated: 2018/06/08 19:50:58 by agrumbac         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

module Calc_int = Droid.Calc(Droid.INT)
module Calc_float = Droid.Calc(Droid.FLOAT)

let () =
	print_endline ("3 ** 3 = " ^ (string_of_int (Calc_int.power 3 3)));
	print_endline ("3 ** 3 = " ^ (string_of_float (Calc_float.power 3.0 3)));
	print_endline ("(20 + 1) * 2 = " ^ (string_of_int (Calc_int.mul (Calc_int.add 20 1) 2)));
	print_endline ("(20 + 1) * 2 = " ^ (string_of_float (Calc_float.mul (Calc_float.add 20.0 1.0) 2.0)));
	print_endline ("(20 - 1 ) / 2 = " ^ (string_of_int (Calc_int.div (Calc_int.sub 20 1) 2)));
	print_endline ("(20 - 1 ) / 2 = " ^ (string_of_float (Calc_float.div (Calc_float.sub 20.0 1.0) 2.0)));
	print_endline ("!5 = " ^ (string_of_int (Calc_int.fact 5)));
	print_endline ("!5 = " ^ (string_of_float (Calc_float.fact 5.0)))
