(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   iter.ml                                            :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: agrumbac <agrumbac@student.42.fr>          +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2018/05/28 17:21:07 by agrumbac          #+#    #+#             *)
(*   Updated: 2018/05/28 17:39:41 by agrumbac         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let rec iter f x n =
	if n < 0 then
		(-1)
	else if n = 0 then
		x
	else
		iter f (f x) (n - 1)

(* ********************************** test ********************************** *)

let () =
	print_int (iter (fun x -> x * x) 2 4);
	print_char '\n';
	print_int (iter (fun x -> x * 2) 2 4);
	print_char '\n';
	print_int (iter (fun x -> x - 2) (-4) 42);
	print_char '\n';
	print_int (iter (fun x -> x / 2) 42 1);
	print_char '\n';
	print_int (iter (fun x -> x * 2) 2 (-1));
	print_char '\n';
	print_int (iter (fun x -> x + 2) 42 4);
	print_char '\n'
