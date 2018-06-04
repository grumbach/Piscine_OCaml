(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   micronap.ml                                        :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: agrumbac <agrumbac@student.42.fr>          +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2018/06/04 14:12:19 by agrumbac          #+#    #+#             *)
(*   Updated: 2018/06/04 16:32:22 by agrumbac         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let my_sleep () = Unix.sleep 1

let goto_sleep time =
	try
		let t = int_of_string time in
		for i = 0 to t - 1 do
			my_sleep ()
		done
	with
	| Failure m -> ()

let () =
	let argv = Array.to_list Sys.argv in
	match argv with
	| hd :: nxt :: tl -> goto_sleep nxt
	| _ -> ()

 (* ocamlc unix.cma micronap.ml && ./a.out 42 *)
