(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   ft_print_alphabet.ml                               :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: agrumbac <agrumbac@student.42.fr>          +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2018/05/27 17:39:38 by agrumbac          #+#    #+#             *)
(*   Updated: 2018/05/27 18:04:42 by agrumbac         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let rec ft_print_letters_from c =
	let n = (int_of_char c) in
	if n > 122 then
		print_char '\n'
	else begin
		print_char c;
		ft_print_letters_from (char_of_int (n + 1))
	end

let ft_print_alphabet () =
	ft_print_letters_from 'a'

(* ********************************** test ********************************** *)

let main () =
	ft_print_alphabet ()

let () = main ()
