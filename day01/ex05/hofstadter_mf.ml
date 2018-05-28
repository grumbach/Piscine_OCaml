(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   hofstadter_mf.ml                                   :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: agrumbac <agrumbac@student.42.fr>          +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2018/05/28 17:11:25 by agrumbac          #+#    #+#             *)
(*   Updated: 2018/05/28 17:20:37 by agrumbac         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let rec hfs_f n =
	if n == 0 then 1
	else
		n - hfs_m (hfs_f (n - 1))

and hfs_m n =
	if n == 0 then 0
	else
		n - hfs_f (hfs_m (n - 1))

(* ********************************** test ********************************** *)

let () =
	print_int (hfs_m 0);
	print_char '\n';
	print_int (hfs_f 0);
	print_char '\n';
	print_int (hfs_m 4);
	print_char '\n';
	print_int (hfs_f 4);
	print_char '\n';
	print_int (hfs_m 42);
	print_char '\n';
	print_int (hfs_f 42);
	print_char '\n'
