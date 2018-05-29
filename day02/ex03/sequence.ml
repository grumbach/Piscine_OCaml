(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   sequence.ml                                        :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: agrumbac <agrumbac@student.42.fr>          +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2018/05/30 15:31:39 by agrumbac          #+#    #+#             *)
(*   Updated: 2018/05/30 17:32:01 by agrumbac         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let rec sequence n =
	if n < 1 then
		""
	else

	let rec sequentialize n string_list =
		if n == 1 then
			string_list
		else
			sequentialize (n - 1) (get_next string_list)

	and get_next string_list =
		match string_list with
		| [] -> []
		| first_elem :: other_elems ->
		let rec find_c_and_count_in l target count =
			match l with
			| [] -> [(string_of_int count) ; target]
			| hd :: tl ->
			if hd = target then
				find_c_and_count_in tl target (count + 1)
			else
				(string_of_int count) :: target :: find_c_and_count_in tl hd 1
		in find_c_and_count_in other_elems first_elem 1

	and string_of_list l =
		match l with
		| [] -> ""
		| hd :: tl -> hd ^ (string_of_list tl)

	in (string_of_list (sequentialize n ["1"])) ^ "\n"


(* ********************************** test ********************************** *)

let () =
	print_string (sequence 1);
	print_string (sequence 2);
	print_string (sequence 3);
	print_string (sequence 4);
	print_string (sequence 5);
	print_string (sequence 6);
	print_string (sequence 7);
	print_string (sequence 8);
	print_string (sequence 9);
	print_string (sequence 10);
	print_string (sequence 11);
	print_string (sequence 12);
	print_string (sequence 13);
	print_string (sequence 14);
	print_string (sequence (-1));
	print_string (sequence 0)
