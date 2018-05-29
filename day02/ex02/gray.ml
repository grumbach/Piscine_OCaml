(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   gray.ml                                            :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: agrumbac <agrumbac@student.42.fr>          +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2018/05/29 20:47:43 by agrumbac          #+#    #+#             *)
(*   Updated: 2018/05/30 15:34:02 by agrumbac         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)


let gray n =

	if n < 1 then
		print_string "\n"
	else

	let rec compute_gray_list iteration previous_list =
		if iteration == 1 then
			previous_list
		else
			compute_gray_list (iteration - 1) (reflect_gray_list previous_list)

	and reflect_gray_list l =
		match l with
		| []		-> []
		| hd :: []	-> ("0" ^ hd) :: [("1" ^ hd)]
		| hd :: tl	-> ("0" ^ hd) :: (reflect_gray_list tl) @ [("1" ^ hd)]

	and string_from_list l =
		match l with
		| [] -> "\n"
		| hd :: tl -> hd ^ " " ^ (string_from_list tl)

	in
	print_string (string_from_list (compute_gray_list n ["0"; "1"]))



(* ********************************** test ********************************** *)

let () =
	gray 12;
	gray (-1);
	gray 0;
	gray 1;
	gray 2;
	gray 3;
	gray 4
