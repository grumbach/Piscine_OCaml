(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   crossover.ml                                       :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: agrumbac <agrumbac@student.42.fr>          +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2018/05/29 18:59:12 by agrumbac          #+#    #+#             *)
(*   Updated: 2018/05/29 22:24:09 by agrumbac         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let rec crossover l1 l2 =
	match l1 with
	| [] -> []
	| hd :: tl ->
	if (cross hd l2) then
		hd :: (crossover tl l2)
	else
		crossover tl l2
and cross item l =
	match l with
	| [] -> false
	| hd :: tl ->
	if hd = item then
		true
	else
		cross item tl

(* ********************************** test ********************************** *)

let () =
	let rec print_int_list l =
		match l with
		| [] -> "[]\n"
		| hd :: tl -> (string_of_int hd) ^ " :: " ^ (print_int_list tl)

	in
	print_string "--------- int ----------\n\n";
	print_string (print_int_list (crossover [12] [4;77;123;5678;12;5;5;5;6]));
	print_string (print_int_list (crossover [4;2;7] [1;2;2;3;3;3;4;4;4;4;5;5;5;5;5]));
	print_string (print_int_list (crossover [12;45;32;4;56;123;56435;12] [45;32;4;12;12;2345;658;24;467;234;674;23;76;234;456;2]));
	print_string (print_int_list (crossover [12;(-1);321;51] [42;(-1);42;1;3;6;8;3;1;42]));
	print_string (print_int_list (crossover [] [42;123;32]));
	print_string (print_int_list (crossover [23;123;12] []));

	let rec print_str_list l =
		match l with
		| [] -> "[]\n"
		| hd :: tl -> "\"" ^ hd ^ "\"" ^ " :: " ^ (print_str_list tl)

	in
	print_string "\n--------- string -------\n\n";
	print_string (print_str_list (crossover ["hey"] ["hey"; "hey"; "hoy"]));
	print_string (print_str_list (crossover ["42"] ["42"; ""; "42"]));
	print_string (print_str_list (crossover ["hey";"ho";"he";"hee";"yo";"aa"] ["hee";"yo";"aa";"hey"]));
	print_string (print_str_list (crossover ["hey"] ["a"; "a"; "l"; "o"; "a"]));
	print_string (print_str_list (crossover ["hey"] ["hoy"]))
