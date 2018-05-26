(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   ft_print_rev.ml                                    :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: agrumbac <agrumbac@student.42.fr>          +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2018/05/27 18:26:23 by agrumbac          #+#    #+#             *)
(*   Updated: 2018/05/27 19:54:01 by agrumbac         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

(* print_char, String.get and String.length *)


let rec ft_print_last_from s l =
	if l >= 0 then begin
		print_char (String.get s l);
		ft_print_last_from s (l - 1)
	end


let ft_print_rev s =
	ft_print_last_from s (String.length s - 1);
	print_char '\n'

(* ********************************** test ********************************** *)

let main () =
	ft_print_rev "PHP ud siaf iuQ";
	ft_print_rev "a";
	ft_print_rev "24";
	ft_print_rev "";
	ft_print_rev "lol"

let () = main ()
