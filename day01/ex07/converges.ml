(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   converges.ml                                       :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: agrumbac <agrumbac@student.42.fr>          +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2018/05/28 17:41:31 by agrumbac          #+#    #+#             *)
(*   Updated: 2018/05/28 18:13:34 by agrumbac         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let rec converges f x n =
	if n < 0 then
		false
	else if (f x) = x then
		true
	else
		converges f (f x) (n - 1)

(* ********************************** test ********************************** *)

let () =
	let test_for f x n =
		if converges f x n then
			print_string "true, found fixed-point\n"
		else
			print_string "false\n"
	in
	test_for (fun x -> x) 42 0;
	test_for (fun x -> x - x) 42 1;
	test_for (fun x -> x / 2) 2 5;
	test_for (fun x -> x / 2) 2 4;
	test_for (fun x -> x / 2) 2 3;
	test_for (fun x -> x / 2) 2 3;
	test_for (fun x -> x / 2) 2 2;

	print_string "\n\n";

	test_for (fun x -> x / 2) 2 1;
	test_for (( * ) 2) 2 5;
	test_for (fun x -> x +. 2.42) 42.0 12;
	test_for (fun x -> x *. 3.14) 126.0 4;
	test_for (fun x -> x + 2) 42 4
