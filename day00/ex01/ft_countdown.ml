(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   ft_countdown.ml                                    :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: agrumbac <agrumbac@student.42.fr>          +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2018/05/27 16:30:37 by agrumbac          #+#    #+#             *)
(*   Updated: 2018/05/27 17:41:04 by agrumbac         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let print_int_endl n =
	print_int n;
	print_char '\n'

let rec ft_countdown n =
	if n <= 0 then
		print_int_endl 0
	else begin
		print_int_endl n;
		ft_countdown (n - 1)
	end

(* ********************************** test ********************************** *)

let main () =
	ft_countdown 42;
	print_char '-';
	print_char '\n';
	ft_countdown 3;
	print_char '-';
	print_char '\n';
	ft_countdown 0;
	print_char '-';
	print_char '\n';
	ft_countdown (-1);
	print_char '-';
	print_char '\n'

let () = main ()
