(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   encode.ml                                          :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: agrumbac <agrumbac@student.42.fr>          +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2018/05/29 17:17:12 by agrumbac          #+#    #+#             *)
(*   Updated: 2018/05/29 18:58:34 by agrumbac         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let encode l =
	match l with
		| [] -> []
		| first_elem :: other_elems ->
	let rec find_c_and_count_in l c n =
		match l with
		| [] -> [(n, c)]
		| hd :: tl ->
		if hd = c then
			find_c_and_count_in tl c (n + 1)
		else
			(n, c) :: find_c_and_count_in tl hd 1

	in find_c_and_count_in other_elems first_elem 1


(* ********************************** test ********************************** *)

let () =
	let rec get_n (n,_) = (string_of_int n)
	and get_c (_,c) = (string_of_int c)
	and print_int_list l =
		match l with
		| [] -> "[end]\n"
		| hd :: tl -> "(" ^ (get_n hd) ^ ":" ^ (get_c hd) ^ ")" ^ (print_int_list tl)

	in
	print_string "--------- int ----------\n\n";
	print_string (print_int_list (encode [4;77;123;5678;12;5;5;5;6]));
	print_string (print_int_list (encode [1;2;2;3;3;3;4;4;4;4;5;5;5;5;5]));
	print_string (print_int_list (encode []));
	print_string (print_int_list (encode [42]));
	print_string (print_int_list (encode [42;(-1);42;1;3;6;8;3;1;42]));


	let rec get_n (n,_) = (string_of_int n)
	and get_s (_,s) = s
	and print_str_list l =
		match l with
		| [] -> "[end]\n"
		| hd :: tl -> "(" ^ (get_n hd) ^ ":\"" ^ (get_s hd) ^ "\")" ^ (print_str_list tl)

	in
	print_string "\n--------- string -------\n\n";
	print_string (print_str_list (encode ["hey"; "hey"; "hoy"]));
	print_string (print_str_list (encode ["a"; "a"; "l"; "o"; "a"]));
	print_string (print_str_list (encode ["42"; ""; "42"]));
	print_string (print_str_list (encode ["hey"]))
