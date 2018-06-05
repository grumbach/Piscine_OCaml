(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   jokes.ml                                           :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: agrumbac <agrumbac@student.42.fr>          +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2018/06/04 19:48:32 by agrumbac          #+#    #+#             *)
(*   Updated: 2018/06/05 14:10:14 by agrumbac         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let () =
	let filename = "jokes.txt" in
	let n = ref 0 in
	let read_file =
		let lines = ref [] in
		let istream = open_in filename in
		try
			while true; do
				incr n;
				lines := input_line istream :: !lines
			done; !lines
		with End_of_file ->
			close_in istream;
			List.rev !lines
	in
	let joke_array = (Array.of_list read_file) in
	Random.self_init ();

	if !n > 1 then
		print_endline (Array.get joke_array (Random.int (!n - 1)))
