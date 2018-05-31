(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   test.ml                                            :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: agrumbac <agrumbac@student.42.fr>          +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2018/05/31 21:29:09 by agrumbac          #+#    #+#             *)
(*   Updated: 2018/06/01 15:16:34 by agrumbac         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

(* ********************************** test ********************************** *)

let () =
	let str= "abcdefghijklmnopqstuvwxyzABCDEFGHIJKLMNOPQSTUVWXYZ"; in
	let crypted = (Cipher.ft_crypt str
		[Cipher.rot42;
		(Cipher.caesar (-503));
		(Cipher.xor (42))]) in
	let decrypted = (Uncipher.ft_uncrypt crypted
		[(Uncipher.xor (42));
		(Uncipher.uncaesar (-503));
		Uncipher.unrot42]) in
	print_endline ("str:       " ^ str);
	print_endline ("crypted:   " ^ crypted);
	print_endline ("decrypted: " ^ decrypted)
