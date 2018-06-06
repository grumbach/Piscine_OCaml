(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   main.ml                                            :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: agrumbac <agrumbac@student.42.fr>          +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2018/06/06 16:44:40 by agrumbac          #+#    #+#             *)
(*   Updated: 2018/06/06 16:44:53 by agrumbac         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let () =
	let tom = new People.people ("Tom")
	and jerry = new People.people ("Jerry")
	in
	tom#talk;
	tom#die;
	jerry#talk;
	print_endline jerry#to_string
