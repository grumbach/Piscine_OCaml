(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   main.ml                                            :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: agrumbac <agrumbac@student.42.fr>          +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2018/06/08 23:07:33 by agrumbac          #+#    #+#             *)
(*   Updated: 2018/06/08 23:07:43 by agrumbac         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

(* this is the base controller *)

let () =
	if (true) then
		View.Graphics_User_Interface.main ()
	else
		View.Shell_User_Interface.main ()
