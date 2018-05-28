(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   ft_sum.ml                                          :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: agrumbac <agrumbac@student.42.fr>          +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2018/05/28 18:21:13 by agrumbac          #+#    #+#             *)
(*   Updated: 2018/05/28 21:38:47 by agrumbac         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let ft_sum f i n =
	if i > n then
		nan
	else let rec actual_sum f i n rax =
		if i == n then
			f i +. rax
		else
			actual_sum f (i + 1) n ((f i) +. rax)
	in actual_sum f i n 0.0

(* ********************************** test ********************************** *)

let () =
	print_float (ft_sum (fun i -> float_of_int (i * i)) 1 10);
	print_char '\n';
	print_float (ft_sum (fun i -> float_of_int (i * 2)) 1 100);
	print_char '\n';
	print_float (ft_sum (fun i -> float_of_int (i + i)) 42 43);
	print_char '\n';
	print_float (ft_sum (fun i -> float_of_int (i * i)) (-1) (-3));
	print_char '\n';
	print_float (ft_sum (fun i -> float_of_int (i * i)) 12 1);
	print_char '\n';
	print_float (ft_sum (fun i -> float_of_int (i * i)) 1 1);
	print_char '\n'
