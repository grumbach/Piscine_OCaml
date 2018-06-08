(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   test.ml                                            :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: agrumbac <agrumbac@student.42.fr>          +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2018/06/08 21:33:21 by agrumbac          #+#    #+#             *)
(*   Updated: 2018/06/08 22:59:54 by agrumbac         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let () =
	let int_of_string_monad s = Try.Try.return (int_of_string s)
	and int_of_exn (e:exn) = Try.Try.return 42
	and cmp_any_false a = false
	and fake_int_filter_muhahahahahaha str x =
		print_endline (str ^ "[int] -> " ^ (string_of_int x));
		true
	and fake_str_filter_muhahahahahaha str s =
		print_endline (str ^ "[str] -> " ^ s);
		true

	in

	let str_int = Try.Try.return "42" in
	let str_fail = Try.Try.return "this is a string" in
	let int_fail = Try.Try.bind str_fail (int_of_string_monad) in
	let int_42 = Try.Try.bind str_int (int_of_string_monad) in
	let rec_int = Try.Try.recover int_fail int_of_exn in
	let fil_fail = Try.Try.filter int_42 cmp_any_false in
	let deep = Try.Try.return str_int in
	let flat = Try.Try.flatten deep

	in

	ignore(Try.Try.filter str_int (fake_str_filter_muhahahahahaha "str_int: "));
	ignore(Try.Try.filter str_fail (fake_str_filter_muhahahahahaha "str_fail: "));
	ignore(Try.Try.filter int_fail (fake_int_filter_muhahahahahaha "int_fail: "));
	ignore(Try.Try.filter int_42 (fake_int_filter_muhahahahahaha "int_42: "));
	ignore(Try.Try.filter rec_int (fake_int_filter_muhahahahahaha "rec_int: "));
	ignore(Try.Try.filter fil_fail (fake_int_filter_muhahahahahaha "fil_fail: "));
	ignore(Try.Try.filter flat (fake_str_filter_muhahahahahaha "flat: "))
